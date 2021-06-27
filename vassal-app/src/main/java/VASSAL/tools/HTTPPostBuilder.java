/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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
package VASSAL.tools;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Random;

/**
 * Builds HTTP POST requests conveniently.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 *
 * @deprecated Use Apache HttpComponents instead. See {@link BugUtils} for an
 * example.
 */
@Deprecated(since = "2021-06-28", forRemoval = true)
public class HTTPPostBuilder {
  private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();
  private final BufferedWriter bw =
    new BufferedWriter(new OutputStreamWriter(bytes, StandardCharsets.UTF_8));

  private final String boundary = "---------------------------" +
    randomString() + randomString() + randomString();

  private static final String endl = "\r\n";

  private static final Random rng = new Random();

  private static String randomString() {
    return Long.toString(rng.nextLong(), Character.MAX_RADIX);
  }

  /**
   * Sets a parameter for an HTTP POST request.
   *
   * @param name the name of the parameter
   * @param value the value of the parameter
   * @throws IOException in case of failure
   */
  public void setParameter(String name, String value) throws IOException {
    bw.append("--")
      .append(boundary)
      .append(endl)
      .append("Content-Disposition: form-data; name=\"") //NON-NLS
      .append(name)
      .append('"')
      .append(endl)
      .append(endl)
      .append(value)
      .append(endl);
  }

  /**
   * Sets a file to be uploaded as part of an HTTP POST request.
   *
   * @param name the name of the parameter
   * @param file the path to the file
   * @throws IOException in case of failure
   */
  public void setParameter(String name, File file) throws IOException {
    try (InputStream in = Files.newInputStream(file.toPath())) {
      setParameter(name, file.getPath(), in);
    }
  }

  /**
   * Sets a file to be uploaded as part of an HTTP POST request.
   *
   * @param name the name of the parameter
   * @param filename the path to the file
   * @param in an <code>InputStream</code> from which to read the file
   * @throws IOException in case of failure
   */
  public void setParameter(String name, String filename, InputStream in)
                                                          throws IOException {
    // write out the headers
    writeCommonFileHeaders(name, filename);

    final String type = HttpURLConnection.guessContentTypeFromName(filename);
    bw.append(type == null ? "application/octet-stream" : type) //NON-NLS
      .append(endl)
      .append(endl);

    // flush before we switch to writing bytes
    bw.flush();

    // write the file to the byte buffer
    in.transferTo(bytes);

    bw.append(endl);
  }

  /**
   * Sets a file to be uploaded as part of an HTTP POST request.
   *
   * @param name the name of the parameter
   * @param filename the path to the file
   * @param contents a <code>String</code> containing the file
   * @throws IOException in case of failure
   */
  public void setParameter(String name, String filename, String contents)
                                                          throws IOException {
    // write out the headers
    writeCommonFileHeaders(name, filename);

    // write out the contents at UTF-8
    bw.append("text/plain; charset=\"UTF-8\"") //NON-NLS
      .append(endl)
      .append(endl)
      .append(contents)
      .append(endl);
  }

  private void writeCommonFileHeaders(String name, String filename)
                                                          throws IOException {
    bw.append("--")
      .append(boundary)
      .append(endl)
      .append("Content-Disposition: form-data; name=\"") //NON-NLS
      .append(name)
      .append("\"; filename=\"") //NON-NLS
      .append(filename)
      .append(endl)
      .append("Content-Type: "); //NON-NLS
  }

  private void writeEnd() throws IOException {
    bw.append("--").append(boundary).append("--").append(endl);
    bw.close();
  }

  /**
   * Submits an HTTP POST request to the given URL.
   * This convenience method is equivalent to
   * <code>HTTPPostBuilder.post(new URL(url))</code>.
   *
   * @param url the URL to receive the POST request
   * @return the reply
   * @throws IOException in case of failure
   */
  public InputStream post(String url) throws IOException {
    return post(new URL(url));
  }

  /**
   * Submits an HTTP POST request to the given URL.
   *
   * @param url the URL to receive the POST request
   * @return the reply
   * @throws IOException in case of failure
   */
  public InputStream post(URL url) throws IOException {
    writeEnd();

    final HttpURLConnection http = (HttpURLConnection) url.openConnection();

    http.setRequestMethod("POST"); //NON-NLS
    http.setDoInput(true);
    http.setDoOutput(true);
    http.setUseCaches(false);
    http.setAllowUserInteraction(false);

    http.setRequestProperty("Content-Type",
      "multipart/form-data; boundary=" + boundary);
    http.setRequestProperty("Content-Length", String.valueOf(bytes.size()));

    try (OutputStream out = http.getOutputStream()) {
      bytes.writeTo(out);
    }

    return http.getInputStream();
  }
}
