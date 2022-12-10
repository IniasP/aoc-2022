#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <fstream>

using namespace std;

class Range {
public:
    int start;
    int end;

    Range(int start, int end): start(start), end(end) {};

    Range(string rangeStr) {
        stringstream rangeStream(rangeStr);
        string startStr;
        string endStr;
        getline(rangeStream, startStr, '-');
        getline(rangeStream, endStr);
        start = stoi(startStr);
        end = stoi(endStr);
    }

    bool contains(Range const& other) const {
        return start <= other.start && end >= other.end;
    }

    static bool overlaps(Range const& range1, Range const& range2) {
        return range1.start <= range2.end && range1.end >= range2.start
            || range2.start <= range1.end && range2.end >= range1.start;
    }

    static bool containsOrIsContainedBy(Range const& range1, Range const& range2) {
        return range1.contains(range2) || range2.contains(range1);
    }
};

vector<pair<Range, Range>> parseInput(string input) {
    stringstream stream(input);
    string line;
    vector<pair<Range, Range>> result;
    while (getline(stream, line, '\n')) {
        stringstream lineStream(line);
        string range1Str;
        string range2Str;
        getline(lineStream, range1Str, ',');
        getline(lineStream, range2Str);
        result.push_back(pair(Range(range1Str), Range(range2Str)));
    }
    return result;
}

string getInput() {
    ifstream ifStream("input");
    stringstream buffer;
    buffer << ifStream.rdbuf();
    return buffer.str();
}

int solve(vector<pair<Range, Range>> const& pairs, bool (*test)(Range const&, Range const&)) {
    int count = 0;
    for (const auto& [range1, range2] : pairs) {
        if (test(range1, range2)) {
            count++;
        }
    }
    return count;
}

int main() {
    // string input = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8";
    const string input = getInput();
    const vector<pair<Range, Range>> pairs = parseInput(input);
    cout << "Solution 1: " << solve(pairs, Range::containsOrIsContainedBy) << endl;
    cout << "Solution 2: " << solve(pairs, Range::overlaps) << endl;
    return 0;
}
