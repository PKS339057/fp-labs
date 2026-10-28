import unittest

from euler import lattice_paths, sum_square_difference


class EulerTest(unittest.TestCase):
    def test_problem_6(self):
        self.assertEqual(sum_square_difference(10), 2640)
        self.assertEqual(sum_square_difference(100), 25164150)

    def test_problem_15(self):
        self.assertEqual(lattice_paths(2), 6)
        self.assertEqual(lattice_paths(20), 137846528820)


if __name__ == "__main__":
    unittest.main()
