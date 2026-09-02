#!/usr/bin/env python3
"""Translate batched text via local Ollama. Runs outside Emacs."""

import json
import re
import sys
import urllib.error
import urllib.request


def chat(url, model, prompt, timeout):
    payload = json.dumps(
        {
            "model": model,
            "stream": False,
            "messages": [{"role": "user", "content": prompt}],
        }
    ).encode("utf-8")
    req = urllib.request.Request(
        url,
        data=payload,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        data = json.loads(resp.read().decode("utf-8"))
    text = (data.get("message") or {}).get("content") or ""
    text = text.strip()
    if not text:
        raise RuntimeError("Ollama returned an empty translation")
    return text


def parse_batch(text, n):
    items = [None] * n
    matches = list(re.finditer(r"(?m)^[ \t]*(\d+)\|\|", text))
    if not matches:
        return None
    for i, m in enumerate(matches):
        idx = int(m.group(1)) - 1
        beg = m.end()
        end = matches[i + 1].start() if i + 1 < len(matches) else len(text)
        if 0 <= idx < n and items[idx] is None:
            items[idx] = text[beg:end].strip()
    if any(x is None for x in items):
        return None
    return items


def tag_note(text):
    if "<t" in text:
        return "保留所有 <t数字>、</t数字>、<t数字/> 标签，一个字符都不要改；只翻译标签内外的普通文字。"
    return ""


def prompt_one(target, text):
    return "将下面文本翻译成%s。只输出译文，不要解释，不要引号。%s\n%s" % (
        target,
        tag_note(text),
        text,
    )


def prompt_batch(target, cores):
    body = "\n".join("%d||%s" % (i, c) for i, c in enumerate(cores, 1))
    return (
        "将下面编号片段分别翻译成%s。每条格式必须是 数字||译文 ，不要解释，不要改编号。%s\n%s"
        % (target, tag_note(body), body)
    )


def translate_cores(url, model, timeout, cores, target):
    if len(cores) == 1:
        return [chat(url, model, prompt_one(target, cores[0]), timeout)]
    parsed = parse_batch(
        chat(url, model, prompt_batch(target, cores), timeout), len(cores)
    )
    if parsed:
        return parsed
    return [chat(url, model, prompt_one(target, core), timeout) for core in cores]


def main():
    if len(sys.argv) != 3:
        sys.stderr.write("usage: trans-worker.py INPUT.json OUTPUT.json\n")
        sys.exit(2)
    with open(sys.argv[1], "r", encoding="utf-8") as fh:
        job = json.load(fh)
    url = job["url"]
    model = job["model"]
    timeout = int(job.get("timeout") or 60)
    batches = job["batches"]
    total = max(1, len(batches))
    out_batches = []
    for i, batch in enumerate(batches, 1):
        sys.stdout.write("PROGRESS %d %d\n" % (i, total))
        sys.stdout.flush()
        target = "英文" if batch.get("to_en") else "简体中文"
        out_batches.append(
            translate_cores(url, model, timeout, batch["cores"], target)
        )
    with open(sys.argv[2], "w", encoding="utf-8") as fh:
        json.dump({"ok": True, "batches": out_batches}, fh, ensure_ascii=False)


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        err = str(exc)
        out = sys.argv[2] if len(sys.argv) > 2 else None
        if out:
            try:
                with open(out, "w", encoding="utf-8") as fh:
                    json.dump({"ok": False, "error": err}, fh, ensure_ascii=False)
            except Exception:
                pass
        sys.stderr.write(err + "\n")
        sys.exit(1)
