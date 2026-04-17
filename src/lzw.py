import struct

CLEAR_CODE = 0x100
END_CODE = 0x101
MAX_CODE = 0xFFFF
INITIAL_CODE = 0x102


def compress_file(file_path, out_file):
    """Compress a file into an LZW-encoded file (16-bit codes, little-endian)."""
    data = _read_bytes(file_path)
    compressed = compress(data)
    _write_words(compressed, out_file)


def decompress_file(file_path, out_file):
    """Decompress an LZW-encoded file back to its original bytes."""
    data = _read_words(file_path)
    decompressed = decompress(data)
    _write_bytes(decompressed, out_file)


def compress(input_bytes):
    """Encode a sequence of 8-bit values into LZW codes."""
    dict_ = _init_dict()
    current_code = INITIAL_CODE
    output = [CLEAR_CODE]
    inp = list(input_bytes)

    while True:
        if current_code > MAX_CODE:
            output.append(CLEAR_CODE)
            dict_ = _init_dict()
            current_code = INITIAL_CODE
            continue

        if not inp:
            output.append(END_CODE)
            return output

        byte = inp[0]
        rest = inp[1:]
        next_byte = rest[0] if rest else None
        next_seq = (byte, next_byte) if next_byte is not None else None

        if next_seq is None or next_seq not in dict_:
            if next_seq is not None:
                dict_[next_seq] = current_code
            current_code += 1
            output.append(byte)
            inp = rest
        else:
            inp = [dict_[next_seq]] + rest[1:]


def decompress(input_bytes):
    """Decode a sequence of LZW codes back to 8-bit values."""
    dict_ = {}
    current_code = INITIAL_CODE
    output = []
    inp = list(input_bytes)

    while inp:
        encoded = inp[0]
        rest = inp[1:]

        byte = dict_.get(encoded)
        next_encoded = rest[0] if rest else None
        next_decoded = dict_.get(next_encoded) if next_encoded is not None else None
        first_of_next = next_decoded[0] if next_decoded else None

        is_regular = encoded < CLEAR_CODE or encoded > END_CODE
        if is_regular and first_of_next is not None:
            dict_[current_code] = (byte or []) + [first_of_next]
        elif byte:
            if current_code not in dict_:
                dict_[current_code] = byte + [byte[0]]

        if encoded == CLEAR_CODE:
            dict_ = _init_d_dict()
            current_code = INITIAL_CODE
        elif encoded == END_CODE:
            pass
        else:
            if byte is not None:
                output.append(byte)
            current_code += 1

        inp = rest

    return [b for sublist in output for b in sublist]


def _init_dict():
    return {(code,): code for code in range(256)}


def _init_d_dict():
    return {code: [code] for code in range(256)}


def _read_bytes(path):
    with open(path, 'rb') as f:
        return list(f.read())


def _write_bytes(data, path):
    with open(path, 'wb') as f:
        f.write(bytes(data))


def _read_words(path):
    with open(path, 'rb') as f:
        raw = f.read()
    count = len(raw) // 2
    return list(struct.unpack_from(f'<{count}H', raw))


def _write_words(data, path):
    with open(path, 'wb') as f:
        f.write(struct.pack(f'<{len(data)}H', *data))
