import { readFileSync } from "fs";

const exampleInput = `\
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw\
`.split(/\r?\n/);

const input = readFileSync("input", "utf-8").split(/\r?\n/);

const splitBags = (inputLines: string[]) =>
  inputLines.map((l) => {
    const half = l.length / 2;
    return [l.slice(0, half), l.slice(half)] as [string, string];
  });

const priorityOrder = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
const toPriority = (c: string) => {
  return priorityOrder.indexOf(c) + 1;
};

const sum = (ns: number[]) => ns.reduce((acc, n) => acc + n, 0);

const getAppearsInBoth = (bag1: string, bag2: string) =>
  [...bag2].find((item) => bag1.includes(item))!;

const solvePart1 = (inp: string[]) =>
  sum(splitBags(inp)
    .map(([bag1, bag2]) => getAppearsInBoth(bag1, bag2))
    .map(toPriority));

console.log(`Part 1 example: ${solvePart1(exampleInput)}`);
console.log(`Part 1: ${solvePart1(input)}`);

const groupPerN = (arr: any[], n: number) => {
  const result: string[][] = [];
  for (let i = 0; i < arr.length; i += n) {
    result.push(arr.slice(i, i + n));
  }
  return result;
};

const getAllAppearingInBoth = (bag1: string, bag2: string) =>
  [...bag2].filter((item) => bag1.includes(item))!;

const getAppearsInAll3 = (bag1: string, bag2: string, bag3: string) => {
  const allAppearingInFirstTwo = getAllAppearingInBoth(bag1, bag2).join(
    ""
  );
  return getAppearsInBoth(allAppearingInFirstTwo, bag3);
};

const solvePart2 = (inp: string[]) => {
  const grouped = groupPerN(inp, 3);
  return sum(grouped.map(([bag1, bag2, bag3]) => getAppearsInAll3(bag1, bag2, bag3)).map(toPriority));
};

console.log(`Part 2 example: ${solvePart2(exampleInput)}`);
console.log(`Part 2: ${solvePart2(input)}`)
