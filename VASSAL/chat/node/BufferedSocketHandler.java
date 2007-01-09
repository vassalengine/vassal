package VASSAL.chat.node;

import java.io.*;
import java.net.Socket;
import java.nio.charset.Charset;

/**
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Aug 27, 2003
 */
public class BufferedSocketHandler extends SocketHandler {
  protected BufferedReader reader;
  protected BufferedWriter writer;

  public BufferedSocketHandler(Socket sock, SocketWatcher handler) throws IOException {
    super(sock, handler);
    reader = new BufferedReader(new InputStreamReader(sock.getInputStream(),Charset.forName("UTF-8")));
    writer = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream(),Charset.forName("UTF-8")));
  }

  protected void closeStreams() throws IOException {
    writer.close();
    reader.close();
  }

  protected String readNext() throws IOException {
    String line = reader.readLine();
    return line;
  }

  protected void writeNext(String line) throws IOException {
    writer.write(line+'\n');
    writer.flush();
  }
}
