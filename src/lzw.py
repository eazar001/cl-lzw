import struct
from collections import deque
from typing import Iterable

CLEAR_CODE = 0x100
END_CODE = 0x101
MAX_CODE = 0xFFFF
INITIAL_CODE = 0x102


def compress_file(file_path: str, out_file: str) -> None:
    with open(file_path, 'rb') as f:
        codes = compress(f.read())

    with open(out_file, 'wb') as f:
        f.write(struct.pack(f'<{len(codes)}H', *codes))


def decompress_file(file_path: str, out_file: str) -> None:
    with open(file_path, 'rb') as f:
        raw = f.read()

    words = list(struct.unpack_from(f'<{len(raw) // 2}H', raw))

    with open(out_file, 'wb') as f:
        f.write(decompress(words))


def compress(input_bytes: Iterable[int]) -> list[int]:
    dict_: dict[tuple[int, ...], int] = {(code,): code for code in range(256)}
    current_code = INITIAL_CODE
    output = [CLEAR_CODE]
    inp: deque[int] = deque(input_bytes)

    while inp:
        if current_code > MAX_CODE:
            output.append(CLEAR_CODE)
            dict_ = {(code,): code for code in range(256)}
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
    dict_: dict[int, list[int]] = {code: [code] for code in range(256)}
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
            dict_ = {code: [code] for code in range(256)}
            current_code = INITIAL_CODE
        elif encoded == END_CODE:
            pass
        else:
            if byte is not None:
                output.extend(byte)
            current_code += 1

    return bytes(output)
