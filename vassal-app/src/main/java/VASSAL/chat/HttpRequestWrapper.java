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
package VASSAL.chat;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;

/**
 * Performs Get and Post operations to a given URL
 */
public class HttpRequestWrapper {
  private String baseURL;

  public HttpRequestWrapper(String baseURL) {
    this.baseURL = baseURL;
  }

  public List<String> doGet(Properties p) throws IOException {
    return doGet("", p); //$NON-NLS-1$
  }

  private List<String> readLines(InputStream is) throws IOException {
    try (InputStreamReader isr = new InputStreamReader(is, StandardCharsets.UTF_8);
         BufferedReader in = new BufferedReader(isr)) {
      final ArrayList<String> l = new ArrayList<>();
      String line;
      while ((line = in.readLine()) != null) l.add(line);

      return l;
    }
  }

  /**
   * Perform a GET request
   * @param url the URL relative to the base URL
   * @param props additional query parameters
   * @return a List of Strings, one for each line in the response
   * @throws IOException
   */
  public List<String> doGet(String url,
                            Properties props) throws IOException {
    url = baseURL + url;
    if (props != null) {
      url += "?"; //$NON-NLS-1$
      for (Enumeration<?> e = props.keys(); e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = props.getProperty(key);
        url += key + "=" + URLEncoder.encode(value,"UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
        if (e.hasMoreElements()) {
          url += "&"; //$NON-NLS-1$
        }
      }
    }

    final URLConnection conn = new URL(url).openConnection();
    conn.setUseCaches(false);
    try (InputStream in = conn.getInputStream()) {
      return readLines(in);
    } 
  }

  public List<String> doPost(Properties p) throws IOException {
    return doPost("",p); //$NON-NLS-1$
  }

  public List<String> doPost(String url,
                             Properties props) throws IOException {
    url = baseURL + url;
    String content = ""; //$NON-NLS-1$
    if (props != null) {
      for (Enumeration<?> e = props.keys(); e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = props.getProperty(key);
        content += key + "=" + URLEncoder.encode(value,"UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
        if (e.hasMoreElements()) {
          content += "&"; //$NON-NLS-1$
        }
      }
    }

    final URLConnection conn = new URL(url).openConnection();
    conn.setDoInput(true);
    conn.setDoOutput(true);
    conn.setUseCaches(false);
    //      conn.setRequestProperty("Content-Type","application/x-www-form-urlencoded");

    try (OutputStream co = conn.getOutputStream();
         DataOutputStream out = new DataOutputStream(co)) {
      out.writeBytes(content);
    }

    try (InputStream in = conn.getInputStream()) {
      return readLines(in);
    } 
  }
}
