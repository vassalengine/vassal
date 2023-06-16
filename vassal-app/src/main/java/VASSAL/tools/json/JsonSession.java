package VASSAL.tools.json;

import java.io.InputStream;
import java.net.URI;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.github.fge.jsonpatch.JsonPatch;

import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.Header;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.message.BasicHeader;
import org.apache.hc.core5.net.URIBuilder;

public class JsonSession {

  public static JsonNode getState(URI uri, Header[] headers) throws Exception {
    final HttpGet httpGet = new HttpGet(uri);
    httpGet.setHeaders(headers);

    try (CloseableHttpClient client = HttpClients.createDefault()) {
      try (CloseableHttpResponse response = client.execute(httpGet)) {
        final int code = response.getCode();
        System.err.println(code);
        if (code == 200) {
          try (InputStream in = response.getEntity().getContent()) {
            return new ObjectMapper().readTree(in);
          }
        }
      }
    }

    return null;
  }

  public static JsonNode postAction(URI uri, String data, Header[] headers) throws Exception {
    final HttpPost httpPost = new HttpPost(uri);
    httpPost.setHeaders(headers);

    final StringEntity requestEntity = new StringEntity(
      data,
      ContentType.APPLICATION_JSON
    );

    httpPost.setEntity(requestEntity);

    try (CloseableHttpClient client = HttpClients.createDefault()) {
      try (CloseableHttpResponse response = client.execute(httpPost)) {
        final int code = response.getCode();
        System.err.println(code);
        if (code == 200) {
          try (InputStream in = response.getEntity().getContent()) {
            return new ObjectMapper().readTree(in);
          }
        }
      }
    }

    return null;
  }

  public static void main(String[] args) throws Exception {
    final URI url = new URIBuilder("http://localhost:8887/state").build();

    final Header[] headers = {
      new BasicHeader("Accept", "application/json"),
      new BasicHeader("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyIjoidWNrZWxtYW4iLCJpYXQiOjE2NzI5NjA0ODd9.3_X-TVxKHYZho-RZ94Lzz9EVKpjVUjHL0bapZ8vVCLc")
    };

    // get the state
    final JsonNode g0 = getState(url, headers).get("g");

    System.out.println(g0.get("draw").get(":cp"));
    System.out.println(g0.get("hand").get(":cp"));
    System.out.println(g0.get("removed").get(":cp"));

    JsonNode r;
    JsonPatch patch;

    // play a card

    final String play = "{\"action\": \"play\", \"args\": [[\"removed\", \":cp\"], [\"hand\", \":cp\", \"0\"]] }";

    r = postAction(url, play, headers);
    patch = JsonPatch.fromJson(r.get("p"));
    final JsonNode g1 = patch.apply(g0);

    System.out.println(g1.get("draw").get(":cp"));
    System.out.println(g1.get("hand").get(":cp"));
    System.out.println(g1.get("removed").get(":cp"));

    // draw a card
    final String draw = "{ \"action\": \"draw\", \"args\": [[\"hand\", \":cp\"], [\"draw\", \":cp\"], \":cp\"] }";

    r = postAction(url, draw, headers);

    patch = JsonPatch.fromJson(r.get("p"));
    final JsonNode g2 = patch.apply(g1);

    System.out.println(g2.get("draw").get(":cp"));
    System.out.println(g2.get("hand").get(":cp"));
    System.out.println(g2.get("removed").get(":cp"));
  }
}
