package org.example;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;

public class Main {
    public static void main(String[] args) throws FileNotFoundException {
        Scanner sc = new Scanner(new File("src/main/resources/input.txt"));
        List<List<String>> parsedOutput = new ArrayList<>();
        List<String> currentList = null;

        while(sc.hasNextLine()) {
            String currentLine = sc.nextLine().trim();

            if (currentLine.startsWith("Monkey")) {
                currentList = new ArrayList<>();
                parsedOutput.add(currentList);
            } else if (!currentLine.isEmpty()) {
                assert currentList != null;
                currentList.add(currentLine);
            }
        }

        List<Monkey> monkeyList = new ArrayList<>();

        for (List<String> list: parsedOutput) {
            String[] startingItems = list.get(0).split(": ")[1].split(", ");
            ArrayList<Integer> worryLevels = new ArrayList<>();

            for (String temp: startingItems) {
                int oldWorry = Integer.parseInt(temp);
                worryLevels.add(oldWorry);
            }

            String[] expression = list.get(1).split(" = ")[1].split(" ");

            Operation newWorry = null;
            if(Objects.equals(expression[1], "+")) {
                int secondOperand = Integer.parseInt(expression[2]);
                newWorry = new AdditionOperation(secondOperand);
            } else if (Objects.equals(expression[1], "*")) {
                if(Objects.equals(expression[2], "old")) {
                    newWorry = new SquareOperation();
                } else {
                    int secondOperand = Integer.parseInt(expression[2]);
                    newWorry = new MultiplyOperation(secondOperand);
                }
            }

            String test = list.get(2).split(": ")[1];
            int testDivisor = Integer.parseInt(test.split(" ")[2]);

            String monkeyForTrueCase = list.get(3).split(": ")[1];
            int monkeyIfDivisible = Integer.parseInt(monkeyForTrueCase.split(" ")[3]);

            String monkeyForFalseCase = list.get(4).split(": ")[1];
            int monkeyIfNotDivisible = Integer.parseInt(monkeyForFalseCase.split(" ")[3]);

            monkeyList.add(
                new Monkey(
                    worryLevels,
                    newWorry,
                    testDivisor,
                    monkeyIfDivisible,
                    monkeyIfNotDivisible
                )
            );
        }

        for (int i = 0; i < 20; i++) {
            for (Monkey monkey : monkeyList) {
                monkey.totalInspectedItems += monkey.worryLevels.size();

                while (monkey.worryLevels.size() > 0) {
                    int oldWorry = monkey.worryLevels.get(0);
                    int newWorry = monkey.newWorry.operate(oldWorry) / 3;

                    if (newWorry % monkey.testDivisor == 0) {
                        monkeyList.get(monkey.MonkeyIfDivisible).worryLevels.add(newWorry);
                    } else {
                        monkeyList.get(monkey.MonkeyIfNotDivisible).worryLevels.add(newWorry);
                    }

                    monkey.worryLevels.remove(0);
                }
            }
        }

        PriorityQueue<Integer> sortedMax = new PriorityQueue<>(Collections.reverseOrder());

        for (Monkey monkey: monkeyList) {
            sortedMax.add(monkey.totalInspectedItems);
        }

        Integer topMost = sortedMax.poll();
        Integer secondPost = sortedMax.poll();

        System.out.println(topMost * secondPost);
    }
}