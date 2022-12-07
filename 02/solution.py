example_input = """\
A Y
B X
C Z
""".splitlines()

values = {
    "X": 1,
    "Y": 2,
    "Z": 3
}

losing_moves = {
    "A": "Z",
    "B": "X",
    "C": "Y"
}

drawing_moves = {
    "A": "X",
    "B": "Y",
    "C": "Z"
}

winning_moves = {
    "A": "Y",
    "B": "Z",
    "C": "X"
}

input = open("input", "r").readlines()

def split_input(lines: list[str]) -> list[tuple[str, str]]:
    return [tuple([s.strip() for s in l.split(" ")]) for l in lines]

example_input = split_input(example_input)
input = split_input(input)

def line_score(line: tuple[str, str]):
    opponent = line[0]
    you = line[1]
    score = values[you]
    if drawing_moves[opponent] == you:
        score += 3
    elif winning_moves[opponent] == you:
        score += 6
    return score

def solve_part1(input_pairs: list[tuple[str, str]]):
    return sum([line_score(l) for l in input_pairs])

print(f"part 1 example: {solve_part1(example_input)}")
print(f"part 1: {solve_part1(input)}")

def what_to_play(line: tuple[str, str]) -> str:
    opponent = line[0]
    outcome = line[1]
    if outcome == "X": # lose
        return losing_moves[opponent]
    elif outcome == "Y": # draw
        return drawing_moves[opponent]
    else: # win
        return winning_moves[opponent]

def part2_to_part1(line: tuple[str, str]) -> tuple[str, str]:
    return (line[0], what_to_play(line))

def solve_part2(input_pairs: list[tuple[str, str]]):
    return solve_part1([part2_to_part1(t) for t in input_pairs])

print(f"part 2 example: {solve_part2(example_input)}")
print(f"part 2: {solve_part2(input)}")
