import struct
from collections import deque
from typing import Iterable

CLEAR_CODE = 0x100
END_CODE = 0x101
MAX_CODE = 0xFFFF
INITIAL_CODE = 0x102


def compress_file(file_path: str, out_file: str) -> None:
    """Compress a file into an LZW-encoded file (16-bit codes, little-endian)."""
    _write_words(compress(_read_bytes(file_path)), out_file)


def decompress_file(file_path: str, out_file: str) -> None:
    """Decompress an LZW-encoded file back to its original bytes."""
    _write_bytes(decompress(_read_words(file_path)), out_file)


def compress(input_bytes: Iterable[int]) -> list[int]:
    """Encode a sequence of 8-bit values into LZW codes."""
    dict_: dict[tuple[int, ...], int] = _init_dict()
    current_code = INITIAL_CODE
    output = [CLEAR_CODE]
    inp: deque[int] = deque(input_bytes)

    while inp:
        if current_code > MAX_CODE:
            output.append(CLEAR_CODE)
            dict_ = _init_dict()
            current_code = INITIAL_CODE

        byte = inp.popleft()
        next_byte = inp[0] if inp else None
        next_seq: tuple[int, int] | None = (byte, next_byte) if next_byte is not None else None

        if next_seq is None or next_seq not in dict_:
            if next_seq is not None:
                dict_[next_seq] = current_code
            current_code += 1
            output.append(byte)
        else:
            inp.popleft()
            inp.appendleft(dict_[next_seq])

    output.append(END_CODE)
    return output


def decompress(input_bytes: Iterable[int]) -> bytes:
    """Decode a sequence of LZW codes back to 8-bit values."""
    dict_: dict[int, list[int]] = {}
    current_code = INITIAL_CODE
    output = bytearray()
    inp: deque[int] = deque(input_bytes)

    while inp:
        encoded = inp.popleft()

        byte = dict_.get(encoded)
        next_encoded = inp[0] if inp else None
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
                output.extend(byte)
            current_code += 1

    return bytes(output)


def _init_dict() -> dict[tuple[int, ...], int]:
    return {(code,): code for code in range(256)}


def _init_d_dict() -> dict[int, list[int]]:
    return {code: [code] for code in range(256)}


def _read_bytes(path: str) -> bytes:
    with open(path, 'rb') as f:
        return f.read()


def _write_bytes(data: bytes, path: str) -> None:
    with open(path, 'wb') as f:
        f.write(data)


def _read_words(path: str) -> list[int]:
    with open(path, 'rb') as f:
        raw = f.read()
    count = len(raw) // 2
    return list(struct.unpack_from(f'<{count}H', raw))


def _write_words(data: list[int], path: str) -> None:
    with open(path, 'wb') as f:
        f.write(struct.pack(f'<{len(data)}H', *data))
