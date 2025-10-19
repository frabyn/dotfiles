#!/usr/bin/env python3
"""
Convert a file to LF line endings and UTF-8 encoding in place.
Usage: convert-lf-utf8 <filename>
"""
import sys

if len(sys.argv) < 2:
    print("Usage: convert-lf-utf8 <filename>")
    sys.exit(1)

try:
    filename = sys.argv[1]

    # Read the file in binary mode
    with open(filename, 'rb') as f:
        content = f.read()

    # Replace CRLF with LF and CR with LF
    content = content.replace(b'\r\n', b'\n').replace(b'\r', b'\n')

    # Convert to UTF-8 (assuming input is Latin-1 or similar if not already UTF-8)
    try:
        text = content.decode('utf-8')
    except UnicodeDecodeError:
        # If not UTF-8, try to decode as Latin-1
        text = content.decode('latin-1')

    # Write back as UTF-8
    with open(filename, 'wb') as f:
        f.write(text.encode('utf-8'))

    print(f"Converted {filename} to LF line endings and UTF-8 encoding")

except Exception as e:
    print(f"Error: {e}", file=sys.stderr)
    sys.exit(1)