package VASSAL.tools.nio.channels;

public interface CompletionHandler<V,A> {
  public void completed(V result, A attachment);

  public void failed(Throwable exc, A attachment);
}
