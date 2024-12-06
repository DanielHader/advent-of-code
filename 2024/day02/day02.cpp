#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

int abs(int val) { return val < 0 ? -val : val; }
int sgn(int val) { return (0 < val) - (val < 0); }

std::vector<std::vector<int>> read_input(const std::string &filename) {
    std::ifstream ifs(filename);
    std::stringstream ss;
    std::string line;
    
    std::vector<std::vector<int>> table;
    
    while (std::getline(ifs, line)) {
        std::vector<int> row;
        int num;

        ss.clear();
        ss.str(line);
        
        while (ss >> num) { row.push_back(num); }
        table.push_back(row);
    }
    ifs.close();
    
    return table;
}

bool is_safe(const std::vector<int> &report) {

    int prev_diff = report[1] - report[0];
    if (abs(prev_diff) < 1 or abs(prev_diff) > 3)
        return false;
        
    for (int i = 2; i < report.size(); i++) {
        int curr_diff = report[i] - report[i-1];

        if (sgn(prev_diff) != sgn(curr_diff)) {
            return false;
        }
        if (abs(curr_diff) < 1 or abs(curr_diff) > 3)
            return false;

        prev_diff = curr_diff;
    }
    return true;
}

int main(int argc, char **argv) {
    auto data = read_input("input.txt");

    int safe_count = 0;
    for (auto &report : data) {
        if (is_safe(report)) { safe_count++; }
    }

    std::cout << "number of safe reports: " << safe_count << std::endl;

    int dampened_safe_count = 0;
    for (auto &report : data) {

        if (is_safe(report)) {
            dampened_safe_count++;
        } else {
            // try removing every element
            // this is slow in general but there are few inputs so who cares
            for (int ignore = 0; ignore < report.size(); ignore++) {
                std::vector<int> dampened_report;
                for (int i = 0; i < report.size(); i++) {
                    if (i != ignore) {
                        dampened_report.push_back(report[i]);
                    }
                }
                if (is_safe(dampened_report)) {
                    dampened_safe_count++;
                    break;
                }
            }
        }
    }

    std::cout << "number of safe reports after damping: " << dampened_safe_count << std::endl;
}
