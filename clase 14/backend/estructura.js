// dataStructures.js
// Implementaciones simples y comentadas para demo/educación

// Nodo para lista simple/doble
class Node {
  constructor(value) {
    this.value = value;
    this.next = null;    // para lista simple
    this.prev = null;    // para lista doble
  }
}

/* -------------------------
   Lista Enlazada Simple (Singly Linked List)
   Usaremos esto en el servidor para almacenar usuarios en memoria
   ------------------------- */
class SinglyLinkedList {
  constructor() {
    this.head = null;
    this.length = 0;
  }

  // Insertar al inicio (como una pila LIFO si quieres)
  insertAtHead(value) {
    const node = new Node(value);
    node.next = this.head;
    this.head = node;
    this.length++;
    return node;
  }

  // Buscar por predicado (ej. user => user.username === 'marcos')
  find(predicate) {
    let cur = this.head;
    while (cur) {
      if (predicate(cur.value)) return cur.value;
      cur = cur.next;
    }
    return null;
  }

  // Recorrer y devolver array (útil para debug)
  toArray() {
    const out = [];
    let cur = this.head;
    while (cur) {
      out.push(cur.value);
      cur = cur.next;
    }
    return out;
  }
}

/* -------------------------
   Lista Doble (Doubly Linked List)
   ------------------------- */
class DoublyLinkedList {
  constructor() {
    this.head = null;
    this.tail = null;
    this.length = 0;
  }

  push(value) {
    const node = new Node(value);
    if (!this.tail) {
      this.head = this.tail = node;
    } else {
      node.prev = this.tail;
      this.tail.next = node;
      this.tail = node;
    }
    this.length++;
    return node;
  }

  pop() {
    if (!this.tail) return null;
    const node = this.tail;
    if (this.tail.prev) {
      this.tail = this.tail.prev;
      this.tail.next = null;
    } else {
      this.head = this.tail = null;
    }
    node.prev = null;
    this.length--;
    return node.value;
  }

  find(predicate) {
    let cur = this.head;
    while (cur) {
      if (predicate(cur.value)) return cur.value;
      cur = cur.next;
    }
    return null;
  }

  toArray() {
    const out = [];
    let cur = this.head;
    while (cur) {
      out.push(cur.value);
      cur = cur.next;
    }
    return out;
  }
}

/* -------------------------
   Pila (Stack) — LIFO
   ------------------------- */
class Stack {
  constructor() {
    this._items = [];
  }
  push(v) { this._items.push(v); }
  pop() { return this._items.pop(); }
  peek() { return this._items[this._items.length - 1]; }
  isEmpty() { return this._items.length === 0; }
  size() { return this._items.length; }
}

/* -------------------------
   Cola (Queue) — FIFO
   ------------------------- */
class Queue {
  constructor() {
    this._items = [];
  }
  enqueue(v) { this._items.push(v); }
  dequeue() { return this._items.shift(); }
  peek() { return this._items[0]; }
  isEmpty() { return this._items.length === 0; }
  size() { return this._items.length; }
}

module.exports = {
  Node,
  SinglyLinkedList,
  DoublyLinkedList,
  Stack,
  Queue
};
