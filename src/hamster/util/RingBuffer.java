package hamster.util;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * Thread safe lock-free ring buffer
 *
 * Constraints:
 *  1) Only one READER thread.
 *
 * @param <E> Element type being stored
 */
public class RingBuffer<E> {
    private final Object[] backing;
    private final int size;
    private final AtomicInteger write;
    private final AtomicInteger read;

    public RingBuffer(final int size) {
        this.backing = new Object[size];
        this.size = size;
        this.write = new AtomicInteger(0);
        this.read = new AtomicInteger(0);
    }

    public void add(E element) {
        backing[write.getAndIncrement() % size] = element;
    }

    @SuppressWarnings("unchecked")
    public E get() {
        return (E) backing[read.getAndIncrement() % size];
    }

    public boolean empty() {
        return read.get() == write.get();
    }
}
