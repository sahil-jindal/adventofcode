package org.example;

public class LinkedList {
    Node head;
    Node tail;

    public LinkedList() {
        this.head = null;
        this.tail = null;
    }

    public void addNode(Integer remainder) {
        Node newNode = new Node(remainder);

        if(head == null) {
            head = newNode;
        } else {
            tail.next = newNode;
        }

        tail = newNode;
    }
}
