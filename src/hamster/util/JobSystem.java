package hamster.util;

import com.google.common.flogger.FluentLogger;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class JobSystem {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Queue<Runnable> jobs = new ArrayDeque<>();
    private static final Worker[] workers;
    private static final int threads = 2;

    static {
        workers = new Worker[2];
        for(int i = 0; i < threads; ++i) {
            workers[i] = new Worker();
            workers[i].start();
        }
    }


    public static class DependencyNotMet extends RuntimeException {
        public DependencyNotMet() {
            super("DependencyNotMet");
        }
    }

    public static class CancelJob extends RuntimeException {
        public CancelJob() {
            super("CancelJob");
        }
    }

    private static class Worker extends Thread {
        private final Queue<Runnable> jobs = new ArrayDeque<>();
        private final AtomicInteger count = new AtomicInteger(0);
        private final AtomicBoolean running = new AtomicBoolean(false);

        private Worker() {
            super("Hafen Job Worker");
            setDaemon(true);
        }

        @Override
        public String toString() {
            final int cnt = count.get();
            final boolean run = running.get();
            return String.format("%d (%s)", cnt, run ? "T" : "F");
        }

        @Override
        public void run() {
            try {
                while (true) {
                    if (jobs.isEmpty()) {
                        final Runnable job = getJob();
                        if (job != null) {
                            jobs.add(job);
                            count.incrementAndGet();
                        } else {
                            waitForJob();
                        }
                    } else {
                        while(!jobs.isEmpty()) {
                            final Runnable job = jobs.poll();
                            count.decrementAndGet();
                            try {
                                running.set(true);
                                job.run();
                                running.set(false);
                            } catch (DependencyNotMet dnm) {
                                jobs.add(job);
                                count.incrementAndGet();
                            } catch (CancelJob can) {
                                logger.atWarning().log("Job has been purposefully canceled");
                            } catch (Exception e) {
                                logger.atSevere().withCause(e).log("Job has failed to run unexpectedly");
                            }
                        }
                    }
                }
            } catch (InterruptedException ie) { }
        }
    }

    private static void waitForJob() throws InterruptedException {
        synchronized (jobs) {
            jobs.wait();
        }
    }

    private static Runnable getJob() {
        synchronized (jobs) {
            if(!jobs.isEmpty()) {
                return jobs.poll();
            } else {
                return null;
            }
        }
    }

    public static void submit(final Runnable job) {
        synchronized (jobs) {
            jobs.add(job);
            jobs.notify();
        }
    }

    public static String status() {
        StringBuilder sb = new StringBuilder();
        for(final Worker w : workers) {
            sb.append(w.toString());
            sb.append(" | ");
        }
        return sb.toString();
    }
}
