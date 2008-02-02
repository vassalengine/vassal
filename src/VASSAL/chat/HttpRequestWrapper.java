/* 
 * $Id$
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
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import VASSAL.tools.IOUtils;

/**
 * Performs Get and Post operations to a given URL
 */
public class HttpRequestWrapper {
  private String baseURL;

  public HttpRequestWrapper(String baseURL) {
    this.baseURL = baseURL;
  }

  public List<String> doGet(Properties p) throws IOException {
    return doGet("",p); //$NON-NLS-1$
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
      for (Enumeration e = props.keys(); e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = props.getProperty(key);
        url += key + "=" + URLEncoder.encode(value,"UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
        if (e.hasMoreElements()) {
          url += "&"; //$NON-NLS-1$
        }
      }
    }
    URL base = new URL(url);
    URLConnection conn = base.openConnection();
    conn.setUseCaches(false);

    final InputStream in = conn.getInputStream();
    final String s;
    try {
      s = IOUtils.toString(in, "UTF-8");
    }
    finally {
      try {
        in.close();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }

    final ArrayList<String> l = new ArrayList<String>();
    final StringTokenizer st = new StringTokenizer(s,"\n\r"); //$NON-NLS-1$
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }
    return l;
  }

  public List<String> doPost(Properties p) throws IOException {
    return doPost("",p); //$NON-NLS-1$
  }

  public List<String> doPost(String url,
                             Properties props) throws IOException {
    url = baseURL + url;
    String content = ""; //$NON-NLS-1$
    if (props != null) {
      for (Enumeration e = props.keys(); e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = props.getProperty(key);
        content += key + "=" + URLEncoder.encode(value,"UTF-8"); //$NON-NLS-1$ //$NON-NLS-2$
        if (e.hasMoreElements()) {
          content += "&"; //$NON-NLS-1$
        }
      }
    }
    URL base = new URL(url);
    URLConnection conn = base.openConnection();
    conn.setDoInput(true);
    conn.setDoOutput(true);
    conn.setUseCaches(false);
    //	    conn.setRequestProperty("Content-Type","application/x-www-form-urlencoded");

    final DataOutputStream out = new DataOutputStream(conn.getOutputStream());
    try {
      out.writeBytes(content);
    }
    finally {
      try {
        out.close();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }

    final BufferedReader input = new BufferedReader(
      new InputStreamReader(conn.getInputStream(), "UTF-8")); //$NON-NLS-1$
    try {
      final ArrayList<String> l = new ArrayList<String>();
      for (String line = input.readLine();
           line != null; line = input.readLine()) {
        l.add(line);
      }
      return l;
    }
    finally {
      try {
        input.close();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
}
