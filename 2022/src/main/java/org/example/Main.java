package org.example;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class Main {

//    public static Pair<Integer, Integer> binarySearch(
//            List<Pair<Integer, Integer>> pairsList,
//            int searchCycle
//    ) {
//        int leftIndex = 0, rightIndex = pairsList.size() - 1;
//        Pair<Integer, Integer> ans = null;
//
//        while(leftIndex <= rightIndex) {
//            int middle = (leftIndex + rightIndex) / 2;
//
//            if(pairsList.get(middle).first() <= searchCycle) {
//                leftIndex = middle + 1;
//                ans = pairsList.get(middle);
//            } else {
//                rightIndex = middle - 1;
//            }
//        }
//
//        return ans;
//    }
    public static void main(String[] args) throws FileNotFoundException {
        Scanner sc = new Scanner(new File("src/main/resources/input.txt"));

        int currentCycle = 0, newResult = 1;

        List<Pair<Integer, Integer>> resultRecord = new ArrayList<>();
        resultRecord.add(Pair.createPair(currentCycle, newResult));

        while(sc.hasNext()) {
            String[] currentline = sc.nextLine().split(" ");

            String operation = currentline[0];
            Pair<Integer, Integer> lastOperation = resultRecord.get(resultRecord.size() - 1);

            if(Objects.equals(operation, "noop")) {
                currentCycle = lastOperation.first() + 1;
                newResult = lastOperation.second();
            } else if (Objects.equals(operation, "addx")) {
                Integer value = Integer.parseInt(currentline[1]);
                currentCycle = lastOperation.first() + 2;
                newResult = lastOperation.second() + value;
            }

            resultRecord.add(Pair.createPair(currentCycle, newResult));
        }

//        int answer = 0;
//
//        for (int cycle = 20; cycle <= 220; cycle += 40) {
//            Pair<Integer, Integer> something = binarySearch(resultRecord, cycle);
//            answer += (cycle * something.second());
//        }
//
//        System.out.println(answer);

        int width = 40;
        int height = 6;
        char[][] grid = new char[height][width];

        for (Pair<Integer, Integer> result: resultRecord) {
            // At each cycle, a pixel is drawn on a CRT TV.
            int cycle = result.first();
            int position = result.second();

            // Relation b/w cycle and pixel.x is pixel.x = cycle - 1;
            // Destruct pixel into rowIndex and columnIndex

            int rowIndex = (cycle - 1) / width;
            int columnIndex = (cycle - 1) % width;

            if(rowIndex >= height) break;

            // now position is actually the middle part of stripe
            // which means there are 3 possible positions -> positions -1, positions, positions + 1

            // Now rowIndex has to be either positions -1, positions, positions + 1
            if(position - 1 >= 0 && position - 1 < width) {
                if (columnIndex == position - 1) {
                    grid[rowIndex][position - 1] = '#';
                }
            }

            if(position>= 0 && position < width) {
                if (columnIndex == position) {
                    grid[rowIndex][position] = '#';
                }
            }

            if(position + 1 >= 0 && position + 1 < width) {
                if (columnIndex == position + 1) {
                    grid[rowIndex][position + 1] = '#';
                }
            }
        }

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                System.out.print(grid[i][j]);
            }

            System.out.println();
        }
    }
}