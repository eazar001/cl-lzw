import tempfile
import unittest
from pathlib import Path
from src.lzw import compress_file, decompress_file

TEXT_FILES = Path(__file__).parent / 'text-files'


class TestLZWRoundTrip(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self._tmp.name)

    def tearDown(self):
        self._tmp.cleanup()

    def _round_trip(self, name: str) -> None:
        original = TEXT_FILES / f'{name}.txt'
        compressed = self.tmp_path / f'{name}-compressed.bin'
        decompressed = self.tmp_path / f'{name}-decompressed.txt'
        compress_file(str(original), str(compressed))
        decompress_file(str(compressed), str(decompressed))
        self.assertEqual(decompressed.read_bytes(), original.read_bytes())

    def test_compress_don_quixote(self):
        self._round_trip('don-quixote')

    def test_compress_moby_dick(self):
        self._round_trip('moby-dick')

    def test_compress_alice_in_wonderland(self):
        self._round_trip('alice-in-wonderland')


if __name__ == '__main__':
    unittest.main()
