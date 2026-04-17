from pathlib import Path
from src.lzw import compress_file, decompress_file

TEXT_FILES = Path(__file__).parent / 'text-files'


def _round_trip(name: str, tmp_path: Path) -> None:
    original = TEXT_FILES / f'{name}.txt'
    compressed = tmp_path / f'{name}-compressed.bin'
    decompressed = tmp_path / f'{name}-decompressed.txt'
    compress_file(str(original), str(compressed))
    decompress_file(str(compressed), str(decompressed))
    assert decompressed.read_bytes() == original.read_bytes()


def test_compress_don_quixote(tmp_path: Path) -> None:
    _round_trip('don-quixote', tmp_path)


def test_compress_moby_dick(tmp_path: Path) -> None:
    _round_trip('moby-dick', tmp_path)


def test_compress_alice_in_wonderland(tmp_path: Path) -> None:
    _round_trip('alice-in-wonderland', tmp_path)
