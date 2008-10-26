package VASSAL.tools.logging;

import java.io.IOException;

import VASSAL.launch.CommandClient;

public class CommandClientAdapter implements LogListener{
  private final CommandClient cmdC;

  public CommandClientAdapter(CommandClient cmdC) {
    this.cmdC = cmdC;
  }

  public void handle(LogEntry entry) {
    try {
      cmdC.request(entry);
    }
    catch (IOException e) {
      // What to do here????
    }
  }
}
