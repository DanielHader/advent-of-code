import java.lang.RuntimeException;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;

import java.util.Comparator;        

class PageComparator implements Comparator<Integer> {

    HashMap<Integer, HashSet<Integer>> orderMap;
    
    public PageComparator(HashMap<Integer, HashSet<Integer>> orderMap) {
        this.orderMap = orderMap;
    }

    // this logic doesn't work in general, but I'm assuming that AoC is giving me logically consistent inputs
    public int compare(Integer a, Integer b) {
        if (a.equals(b)) { return 0; }

        if (this.orderMap.containsKey(a) && this.orderMap.get(a).contains(b)) {
            return -1;
        } else if (this.orderMap.containsKey(b) && this.orderMap.get(b).contains(a)) {
            return 1;
        } else {
            return 0;
        }
    }
}

public class Day05 {

    HashMap<Integer, HashSet<Integer>> orderMap;
    ArrayList<ArrayList<Integer>> updatesList;

    public Day05() {
        this.orderMap = new HashMap<Integer, HashSet<Integer>>();
        this.updatesList = new ArrayList<ArrayList<Integer>>();
    }

    public void parseInput(String filename) throws IOException, NumberFormatException, RuntimeException {
        BufferedReader br = new BufferedReader(new FileReader(filename));
        boolean readingUpdates = false;
        for (String line = br.readLine(); line != null; line = br.readLine()) {

            if (readingUpdates) {
                ArrayList<Integer> updates = new ArrayList<Integer>();
                String[] updateStrings = line.split(",");

                for (String update : updateStrings) {
                    updates.add(Integer.parseInt(update));
                }

                updatesList.add(updates);
            } else {
                if (line.length() == 0) {
                    readingUpdates = true;
                } else {
                    String[] orderRuleStrings = line.split("\\|");
                    if (orderRuleStrings.length != 2) {
                        throw new RuntimeException("Order rule in unexpected format");
                    }

                    int first = Integer.parseInt(orderRuleStrings[0]);
                    int second = Integer.parseInt(orderRuleStrings[1]);

                    if (this.orderMap.containsKey(first)) {
                        this.orderMap.get(first).add(second);
                    } else {
                        HashSet<Integer> hs = new HashSet<Integer>();
                        hs.add(second);
                        this.orderMap.put(first, hs);
                    }
                }
            }
        }

        br.close();
    }

    private boolean inCorrectOrder(int a, int b) {
        if (this.orderMap.containsKey(b) && this.orderMap.get(b).contains(a)) {
            return false;
        } else {
            return true;
        }
    }

    public int getPart1Solution() {
        int result = 0;
        for (ArrayList<Integer> updates : this.updatesList) {
            boolean found = true;
            int middle = 0;
            
            for (int i = 0; i < updates.size() - 1; i++) {
                if (i == updates.size() / 2) { middle = updates.get(i); }
                for (int j = i + 1; j < updates.size(); j++) {
                    
                    int first = updates.get(i).intValue();
                    int second = updates.get(j).intValue();

                    if (!this.inCorrectOrder(first, second)) {
                        found = false;
                        break;
                    }
                }
                if (!found) { break; }
            }
            if (found) { result += middle; }
        }
        return result;
    }

    public int getPart2Solution() {
        int result = 0;

        PageComparator pc = new PageComparator(this.orderMap);
        
        for (ArrayList<Integer> updates : this.updatesList) {
            updates.sort(pc);
            result += updates.get(updates.size() / 2);
        }
        
        return result;
    }
    
    public static void main(String[] args) {

        Day05 day05 = new Day05();

        try {
            day05.parseInput("input.txt");
        } catch (IOException e) {
            System.out.println("Failed to open file");
        } catch (NumberFormatException e) {
            System.out.println("Failed to parse integer from string");
        } catch (RuntimeException e) {
            System.out.println(e.getMessage());
        }

        int p1 = day05.getPart1Solution();
        System.out.println(p1);
        System.out.println(day05.getPart2Solution() - p1);
        
    }
}
