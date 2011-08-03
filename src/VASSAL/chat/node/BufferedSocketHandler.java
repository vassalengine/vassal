/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.chat.node;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
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
    reader = new BufferedReader(new InputStreamReader(sock.getInputStream(),Charset.forName("UTF-8"))); //$NON-NLS-1$
    writer = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream(),Charset.forName("UTF-8"))); //$NON-NLS-1$
  }

  protected void closeStreams() throws IOException {
    writer.close();
    reader.close();
  }

  protected String readNext() throws IOException {
    return reader.readLine();
  }

  protected void writeNext(String line) throws IOException {
    writer.write(line+'\n');
    writer.flush();
  }
}
