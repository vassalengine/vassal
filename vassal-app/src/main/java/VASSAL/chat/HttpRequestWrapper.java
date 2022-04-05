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

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.IOUtils;

import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.apache.hc.core5.net.URIBuilder;

/**
 * Performs Get and Post operations to a given URL
 */
public class HttpRequestWrapper {
  private final String baseURL;

  public HttpRequestWrapper(String baseURL) {
    this.baseURL = baseURL;
  }

  public List<String> doGet(Properties p) throws IOException {
    return doGet("", p); //$NON-NLS-1$
  }

  private URI buildGet(String path, Properties props) throws IOException {
    try {
      final URIBuilder b = new URIBuilder(baseURL + path);
      if (props != null) {
        for (final Enumeration<?> e = props.keys(); e.hasMoreElements();) {
          final String key = (String) e.nextElement();
          final String value = props.getProperty(key);
          b.addParameter(key, value);
        }
      }

      return b.build();
    }
    catch (URISyntaxException e) {
      // this should not happen
      throw new IOException(e);
    }
  }

  private String errorMessage(CloseableHttpResponse response) throws IOException {
    final String msg = response.getCode() + " " + response.getReasonPhrase();

    String responseText = null;
    try {
      responseText = EntityUtils.toString(response.getEntity());
    }
    catch (ParseException e) {
      throw new IOException(msg, e);
    }

    return msg + ": " + responseText;
  }

  /**
   * Perform a GET request
   * @param url the URL relative to the base URL
   * @param props additional query parameters
   * @return a List of Strings, one for each line in the response
   * @throws IOException
   */
  public List<String> doGet(String path,
                            Properties props) throws IOException {
    final HttpGet httpGet = new HttpGet(buildGet(path, props));
    try (CloseableHttpClient client = HttpClients.createDefault()) {
      try (CloseableHttpResponse response = client.execute(httpGet)) {
        return getLinesOk(response, 200);
      }
    }
    catch (final IOException e) {
      throw new IOException("Failed to " + httpGet.toString(), e);
    }
  }

  public List<String> doPost(Properties p) throws IOException {
    return doPost("", p); //$NON-NLS-1$
  }

  private HttpPost buildPost(String path, Properties props) throws IOException {
    final HttpPost httpPost = new HttpPost(baseURL + path);
    final List<NameValuePair> params = new ArrayList<>();
    for (final Enumeration<?> e = props.keys(); e.hasMoreElements();) {
      final String key = (String) e.nextElement();
      final String value = props.getProperty(key);
      params.add(new BasicNameValuePair(key, value));
    }
    httpPost.setEntity(new UrlEncodedFormEntity(params));
    return httpPost;
  }

  public List<String> doPost(String path,
                             Properties props) throws IOException {
    final HttpPost httpPost = buildPost(path, props);
    try (CloseableHttpClient client = HttpClients.createDefault()) {
      try (CloseableHttpResponse response = client.execute(httpPost)) {
        return getLinesOk(response, 201);
      }
    }
  }

  private List<String> getLinesOk(CloseableHttpResponse response, int ok_code) throws IOException {
    if (response.getCode() == ok_code) {
      try {
        return IOUtils.readLines(
          response.getEntity().getContent(),
          StandardCharsets.UTF_8
        );
      }
      catch (UnsupportedOperationException e) {
        throw new IOException(e);
      }
    }
    else {
      throw new IOException(errorMessage(response));
    }
  }
}
