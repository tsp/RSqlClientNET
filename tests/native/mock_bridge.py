#!/usr/bin/env python3
"""Test helper for the native pipe transport; never opens SQL Server."""
import json, os, struct, sys, time

def frame(kind, data):
    sys.stdout.buffer.write(kind + struct.pack('<I', len(data)) + data)
    sys.stdout.buffer.flush()

for line in sys.stdin.buffer:
    req = json.loads(line)
    op = req.get('op')
    if op == 'ping':
        frame(b'J', json.dumps({'ok':True,'kind':'pong','reply':'pong'}).encode())
    elif op == 'large':
        frame(b'D', b'x' * 1048576)
    elif op == 'partial':
        sys.stdout.buffer.write(b'D' + struct.pack('<I', 100) + b'x'*4)
        sys.stdout.buffer.flush()
        time.sleep(30)
    elif op == 'hang':
        time.sleep(30)
    elif op == 'bad':
        frame(b'Q', b'invalid')
    elif op == 'quit':
        break
