package VASSAL.tools.concurrent;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class Exec {
  protected Exec() {}

  public static ExecutorService ex = new ThreadPoolExecutor(
    Runtime.getRuntime().availableProcessors(),
    Runtime.getRuntime().availableProcessors()+1,
    60, TimeUnit.SECONDS,
    new LinkedBlockingQueue<Runnable>()
  );

/*
  public static <T> T submitAndWait(Callable<T> c) {
    try {
      return ex.submit(c).get();
    }
    catch (CancellationException e) {
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      ErrorDialog.bug(e);
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }

  }
*/
}
