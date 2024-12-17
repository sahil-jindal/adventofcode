package org.example;

public record Pair<K, V>(K first, V second) {
    public static <K, V> Pair<K, V> createPair(K first, V second) {
        return new Pair<>(first, second);
    }

    @Override
    public String toString() {
        return "Pair{" + "first=" + first + ", second=" + second + '}';
    }
}
