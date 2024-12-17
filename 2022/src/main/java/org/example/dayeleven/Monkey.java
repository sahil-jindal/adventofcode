package org.example;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class Monkey {
    List<Integer> worryLevels;
    Operation newWorry;
    int testDivisor;
    int MonkeyIfDivisible;
    int MonkeyIfNotDivisible;
    int totalInspectedItems = 0;

    private final LinkedList remainders;

    Monkey(
            ArrayList<Integer> worryLevels,
            Operation newWorry,
            int testDivisor,
            int monkeyIfDivisible,
            int monkeyIfNotDivisible
    ) {
        this.worryLevels = worryLevels;
        this.newWorry = newWorry;
        this.testDivisor = testDivisor;
        this.MonkeyIfDivisible = monkeyIfDivisible;
        this.MonkeyIfNotDivisible = monkeyIfNotDivisible;

        int latestRemainder = 1;
        int base = 10;

        LinkedHashSet<Integer> possibleRemainders = new LinkedHashSet<>();

        while(!possibleRemainders.contains(latestRemainder)) {
            possibleRemainders.add(latestRemainder);
            latestRemainder = (base * latestRemainder) % testDivisor;
        }

        remainders = new LinkedList();

        for(Integer remainder: possibleRemainders) {
            remainders.addNode(remainder);
        }

        possibleRemainders.clear();

        Node tempHead = remainders.head;

        while(tempHead != null) {
            if(tempHead.value == latestRemainder) {
                remainders.tail.next = tempHead;
                break;
            }

            tempHead = tempHead.next;
        }
    }

/*
    public boolean checkIfDivisible(String dividend, int divisor) {
    	// for(Character digit: dividend.toCharArray()) {}
        return false;
    }
*/

    @Override
    public String toString() {
        return "Monkey{" +
            "worryLevels=" + worryLevels +
            ", newWorry=" + newWorry +
            ", testDivisor=" + testDivisor +
            ", MonkeyIfDivisible=" + MonkeyIfDivisible +
            ", MonkeyIfNotDivisible=" + MonkeyIfNotDivisible +
            ", TotalInspectedItems=" + totalInspectedItems +
            '}';
    }
}
