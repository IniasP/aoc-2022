import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

public class Solution {
    public static void main(String[] args) throws IOException {
        List<List<String>> groups = groupLines(Files.lines(Path.of("input")));
        List<Integer> topThree = groups.stream()
                .map(group -> group.stream()
                        .mapToInt(Integer::parseInt)
                        .boxed()
                        .reduce((i1, i2) -> i1 + i2)
                        .orElse(0))
                .sorted(Comparator.reverseOrder())
                .limit(3)
                .toList();
        System.out.println(String.format("Top elf: %d calories", topThree.get(0)));
        Integer topThreeSum = topThree.stream()
                .reduce((i1, i2) -> i1 + i2)
                .orElse(0);
        System.out.println(String.format("Sum of the top 3 elves: %d calories", topThreeSum));
    }

    private static List<List<String>> groupLines(Stream<String> lines) {
        List<List<String>> groups = new ArrayList<>();
        groups.add(new ArrayList<>());
        lines.forEach(line -> {
            if (line.isEmpty()) {
                groups.add(new ArrayList<>());
            } else {
                groups.get(groups.size() - 1).add(line);
            }
        });
        return groups;
    }
}
