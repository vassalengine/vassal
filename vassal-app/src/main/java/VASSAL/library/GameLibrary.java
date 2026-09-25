/*
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.library;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.DigestInputStream;
import java.text.SimpleDateFormat;
import java.text.ParseException;
import java.util.Comparator;
import java.util.Collections;
import java.util.Date;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.io.Console;

import com.github.zafarkhaja.semver.Version;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.apache.hc.core5.http.HttpHeaders;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
// import org.apache.commons.lang3.time.StopWatch;

import VASSAL.Info;
import VASSAL.i18n.Resources;

/**
 * Reads from the VASSAL game library and downloads its files.
 *
 */
public class GameLibrary {

  /** The public API the library website uses. */
  protected static final String DEFAULT_API =
    "https://vassalengine.org/api/gls/v1"; //NON-NLS
  protected static final String[] DATE_FORMATS = {
    "yyyy-MM-dd'T'HH:mm:ssX", //NON-NLS
    "yyyy-MM-dd'T'HH:mm:ss.SSX" //NON-NLS
  };
  protected static final String RESOURCE_FILE =
    "/VASSAL/library/projects.json"; //NON-NLS
  private static final int MAX_PROJECTS = 100;
  private        final URL apiBase;

  public GameLibrary() throws MalformedURLException {
    this(DEFAULT_API);
  }

  public GameLibrary(String apiURL) throws MalformedURLException {    
    apiBase = new URL(apiURL);
  }
  
  protected URL appendPath(String path)
    throws URISyntaxException, MalformedURLException {
    final URI    uri      = apiBase.toURI();
    final String fullPath = uri.getPath() + "/" + path; //NON-NLS
    return uri.resolve(fullPath).toURL();
  }
  protected static URL appendQuery(URL url, String query)
    throws URISyntaxException, MalformedURLException {
    if (query == null || query.isBlank())
      return url;
    return new URL(url.toString() + query);
  }

  protected static Date parseDate(String s) throws ParseException {
    try {
      return (new SimpleDateFormat(DATE_FORMATS[0])).parse(s);
    }
    catch (ParseException ignored) {
    }
    return (new SimpleDateFormat(DATE_FORMATS[1])).parse(s); 
  }

  protected static String formatDate(Date d) throws ParseException {
    return (new SimpleDateFormat(DATE_FORMATS[0])).format(d); 
  }
  
    
  /**
   * Do a GET request
   */
  protected InputStream getRequest(URL url, String accept)
    throws IOException, URISyntaxException {
    
    final HttpGet               get      = new HttpGet(url.toURI());
    get.setHeader(HttpHeaders.USER_AGENT, "VASSAL"); //NON-NLS
    if (accept != null && !accept.isBlank()) 
      get.setHeader(HttpHeaders.ACCEPT, accept);
    
    final CloseableHttpClient   client   = HttpClients.createDefault();
    final CloseableHttpResponse response = client.execute(get);
    final int                   code     = response.getCode();
    
    if (code == 404) {
      throw new IOException(Resources.getString("LibraryBrowser.404", url)); //NON-NLS
    }
    
    if (code / 100 != 2)  {
      throw new IOException(Resources.getString("LibraryBrowser.bad_code", url, code)); //NON-NLS
    }
    
    return response.getEntity().getContent();
  }
  
  protected String getJSON(URL url)  throws IOException, URISyntaxException {
    return new String(getRequest(url, "application/json").readAllBytes(), StandardCharsets.UTF_8); //NON-NLS
  }

  /**
   * Copy response input stream to an output stream, and possibly
   * determine the message digest (check-sum) at the same time.
   */
  protected long copyResponse(InputStream   in,
                              OutputStream  out,
                              MessageDigest digest)
    throws IOException, URISyntaxException {
    if (digest != null) {
      final DigestInputStream din = new DigestInputStream(in, digest);
      in                          = din;
    }
    return in.transferTo(out);
  }

  /**
   * Download a file from the web-server.  The download will check
   * that the recieved file has the expected length, and that the
   * checksum of the downloaded file matches that written in the
   * database.
   */
  protected void getFile(URL url, File outname, String sha256, long size)
    throws IOException, URISyntaxException {
    MessageDigest digest = null;
    try {
      if (sha256 != null && !sha256.isBlank())
        digest = MessageDigest.getInstance("SHA-256"); //NON-NLS
    }
    catch (Exception ignored) {
    }
    
    final InputStream  in   = getRequest(url, null);
    final OutputStream out  = Files.newOutputStream(outname.toPath());
    final long         read = copyResponse(in, out, digest);

    if (read != size)  {
      throw new IOException(Resources.getString("LibraryBrowser.inconsistent_read", //NON-NLS
                                                read, size));
    }
    
    // Check sha-256 against message digest
    if (digest != null && sha256 != null && !sha256.isBlank()) {
      final byte[] expect = HexFormat.of().parseHex(sha256);
      final byte[] got    = digest.digest();
      if (!MessageDigest.isEqual(expect, got)) {
        throw new IOException(Resources.getString("LibraryBrowser.inconsistent_checksum", //NON-NLS
                                                  sha256,
                                                  HexFormat.of().formatHex(got)));
      }
    }
  }

  /**
   * Read in cached projects.  Try first to read from a file in the
   * VASSAL configuration directory.  If that does not exist, try to
   * read from a JAR resource.  If neither is found, return the date
   * of Epoch (1. of January, 1970) so that all retrieved modules are
   * considered newer than that date.
   */
  public Date readCachedProjects(Map<String, Project> ret)
    throws IOException, ParseException {
    final File         confDir = Info.getConfDir();
    final Path         path    = Path.of(confDir.getPath(),
                                         "projects.json"); //NON-NLS
    InputStream        in      = null;
    try {
      in = Files.newInputStream(path);
    }
    catch (IOException ignored) {
      in = null;
    }
      
    if (in == null) 
      in = GameLibrary.class.getResourceAsStream(RESOURCE_FILE);

    if (in == null)
      return new Date(0); // Epoch

    return readCachedProjects(in, ret);
  }
  
    
  /**
   * Read in cached projects from an input stream.  
   */
  public Date readCachedProjects(InputStream          input,
                                 Map<String, Project> ret)
    throws IOException, ParseException {
    Date               last    = new Date(0); // Epoch
    final ObjectMapper mapper  = new ObjectMapper();
    final JsonNode     data    = mapper.readTree(input);
    final ArrayNode    array   = (ArrayNode)data;

    for (final JsonNode project : array) {
      final JsonNode slug    = project.get("slug"); //NON-NLS
      final JsonNode name    = project.get("name"); //NON-NLS
      final JsonNode date    = project.get("modified_at"); //NON-NLS
      final JsonNode title   = project.get("title"); //NON-NLS
      final Date     pdate   = parseDate(date.asText());

      ret.put(slug.asText(), new Project(name.asText(),
                                         title.asText(),
                                         pdate));
      if (pdate.after(last))
        last = pdate;
    }

    return last;
  }

  /**
   * Write cached projects to disk.  The cache is written to the users
   * Vassal configuration directory (e.g., ~/.VASSAL/projects.json on
   * Linux).
   */
  public void writeCachedProjects(Map<String, Project> ret) 
    throws IOException, ParseException {
    final File         confDir = Info.getConfDir();
    final Path         path    = Path.of(confDir.getPath(),
                                         "projects.json"); //NON-NLS
    final File         file    = path.toFile();
    // Make back-up before overwriting? 

    final ObjectMapper mapper  = new ObjectMapper();
    final ArrayNode    data    = mapper.createArrayNode();
    
    for (final String slug : ret.keySet()) {
      final Project     project = ret.get(slug);
      final ObjectNode  object  = mapper.createObjectNode();
      object.put("slug", slug);
      object.put("name", project.getName());
      object.put("title", project.getTitle());
      object.put("modified_at", formatDate(project.getDate()));
      data.add(object);
    }

    mapper.enable(SerializationFeature.INDENT_OUTPUT);
    mapper.writeValue(file, data);
  }
  
  /**
   * Get the list of projects.  This retrieves all projects - not
   * already cached - as fast as possible (100 at a time).
   */
  public Map<String, Project> getProjects()
    throws URISyntaxException,
           MalformedURLException,
           IOException,
           IllegalArgumentException,
           ParseException {
    return getProjects(-1);
  }
  /**
   * Get the list of projects.  This retrieves up to a maximum of
   * projects as fast as possible (100 at a time).
   */
  public Map<String, Project> getProjects(int max)
    throws URISyntaxException,
           MalformedURLException,
           IOException,
           IllegalArgumentException,
           ParseException {
    return getProjects(max, MAX_PROJECTS);
  }
  
  /**
   * Get the list of projects.  This will get up to a maximum of
   * entries, or all if max=-1, at a speed of limit per query.
   */
  public Map<String, Project> getProjects(int max, int limit)
    throws URISyntaxException,
           MalformedURLException,
           IOException,
           IllegalArgumentException,
           ParseException {
    final Map<String, Project> ret  = new LinkedHashMap<>();

    // Perhaps parse `sort_by=m` to get latest modified first. 
    limit                           = Math.min(limit, MAX_PROJECTS);
    final URL          url          = appendPath("projects"); //NON-NLS
    int                fetched      = 0;
    final Date         mark         = readCachedProjects(ret);
    boolean            more         = true;
    final ObjectMapper objectMapper = new ObjectMapper();
    JsonNode           next         =
      objectMapper.readTree("{\"next_page\":\"?limit=" + limit +  //NON-NLS
                            "&sort_by=m\"}").get("next_page"); //NON-NLS
    
    
    // Perhaps parse `sort_by=m` to get latest modified first. 
    while ((max < 0 || fetched < max) && !next.isNull() && more) {
      final URL       nextUrl      = appendQuery(url, next.asText());
      // System.out.println("URL=" + nextUrl);
      final String    response     = getJSON(nextUrl);
      final JsonNode  data         = objectMapper.readTree(response);
      
      // System.out.println(jsonNode);
      // final JSON.Item data     = JSON.parse(response);
      final JsonNode  meta     = data.get("meta"); //NON-NLS
      final int       total    = meta.get("total").asInt(); //NON-NLS
      next                     = meta.get("next_page"); //NON-NLS
      
      final ArrayNode array    = (ArrayNode)data.get("projects"); //NON-NLS
      for (final JsonNode project : array) {
        final JsonNode slug    = project.get("slug"); //NON-NLS
        final JsonNode name    = project.get("name"); //NON-NLS
        final JsonNode date    = project.get("modified_at"); //NON-NLS
        final JsonNode game    = project.get("game"); //NON-NLS
        final JsonNode title   = game.get("title"); //NON-NLS
        final Date     pdate   = parseDate(date.asText());
        fetched++;
        
        if (pdate.before(mark)) {
          more = false;
          break;
        }
        
        ret.put(slug.asText(), new Project(name.asText(),
                                           title.asText(),
                                           pdate));
        
        if (fetched >= total) 
          break;
        
      }
    }
    writeCachedProjects(ret);
      
    return ret;
  }

  /**
   * Get details of a single project.
   *
   * @param slug  Short identifier of the project
   * @param project Fill into this structure
   * @return the project after reading all the details. 
   */
  public Project getProject(String slug, Project project)
    throws URISyntaxException,
           MalformedURLException,
           IOException,
           ParseException {
    final URL          url          = appendPath("projects/" + slug); //NON-NLS
    final String       response     = getJSON(url);
    final ObjectMapper objectMapper = new ObjectMapper();
    final JsonNode     data         = objectMapper.readTree(response);
    final ArrayNode    packages     = (ArrayNode)data.get("packages"); //NON-NLS

    for (final JsonNode pack : packages) {
      final JsonNode  name   = pack.get("name"); //NON-NLS
      final ArrayNode rels   = (ArrayNode)pack.get("releases"); //NON-NLS
      final Package   packge = new Package(name.asText());
      project.addPackage(packge);

      for (final JsonNode rel : rels) {
        final JsonNode  vers  = rel.get("version"); //NON-NLS
        final ArrayNode fs    = (ArrayNode)rel.get("files"); //NON-NLS
        final Release release = new Release(Version.valueOf(vers.asText()));
        packge.addRelease(release);

        for (final JsonNode f : fs) {
          final JsonNode    fn = f.get("filename"); //NON-NLS
          final JsonNode    sh = f.get("sha256"); //NON-NLS
          final JsonNode    re = f.get("requires"); //NON-NLS
          final JsonNode    da = f.get("published_at"); //NON-NLS
          final JsonNode    sz = f.get("size"); //NON-NLS
          final JsonNode    ul = f.get("url"); //NON-NLS
          final String      rq = (re.isNull() ? null :
                                 re.asText().replace(">= ", "")); //NON-NLS
          final Version     rv = rq == null ? null : Version.valueOf(rq);
          final LibraryFile lf = new LibraryFile(fn.asText(),
                                                 new URL(ul.asText()),
                                                 sz.asLong(),
                                                 sh.asText(),
                                                 parseDate(da.asText()),
                                                 release.getNumber(),
                                                 rv);
          release.addFile(lf);
        }
      }
    }
    
    return project;
  }

  /**
   * Download a single file from the library. Note, the file is first
   * written to a temporary file, and only after checking the file
   * size and checksum against what is in the database, do we move it
   * in place.
   *
   * @param file Database entry describing the file
   * @param outputDir Directory to place the file in
   * @return Pointer to downloaded file or null
   */
  public File downloadFile(LibraryFile file, File outputDir)
    throws IOException, URISyntaxException  {

    // Destination and temporary file
    final String fn     = file.getFileName();
    final File   target = new File(outputDir, fn);
    final File   tmp    = File.createTempFile(fn + ".part", //NON-NLS
                                              outputDir.getName()); 

    try {
      getFile(file.getURL(), tmp, file.getChecksum(), file.getSize());

      try {
        Files.move(tmp.toPath(), target.toPath(),
                   StandardCopyOption.ATOMIC_MOVE,
                   StandardCopyOption.REPLACE_EXISTING);
      }
      catch (IOException e) {
        Files.move(tmp.toPath(), target.toPath(),
                   StandardCopyOption.REPLACE_EXISTING);
      }
      return target;
    }
    finally {
      Files.deleteIfExists(tmp.toPath());
    }
  }

  // _________________________________________________________________
  public abstract static class AbstractProjectsComparator
    implements Comparator<Entry<String, Project>> {

    @Override
    public int compare(Entry<String, Project> lhs,
                       Entry<String, Project> rhs) {
      return compareTo(lhs.getValue(), rhs.getValue());
    }
    protected abstract int compareTo(Project lhs, Project rhs);
  }
  // -------------------------------------------------------------------
  public static AbstractProjectsComparator getSlugComparator(boolean asc) {
    return new AbstractProjectsComparator() {
      @Override
      public int compare(Entry<String, Project> lhs,
                            Entry<String, Project> rhs) {
        if (asc) 
          return lhs.getKey().compareTo(rhs.getKey());
        return rhs.getKey().compareTo(lhs.getKey());
      }
      @Override
      protected int compareTo(Project lhs, Project rhs) {
        return 0;
      }
    };
  }
  // -------------------------------------------------------------------
  public static AbstractProjectsComparator getTitleComparator(boolean asc) {
    return new AbstractProjectsComparator() {
      @Override
      protected int compareTo(Project lhs, Project rhs) {
        if (asc)
          return lhs.getTitle().compareTo(rhs.getTitle());
        return rhs.getTitle().compareTo(lhs.getTitle());
      }
    };
  }
  // -------------------------------------------------------------------
  public static AbstractProjectsComparator getDateComparator(boolean asc) {
    return new AbstractProjectsComparator() {
      @Override
      protected int compareTo(Project lhs, Project rhs) {
        if (asc)
          return lhs.getDate().compareTo(rhs.getDate());
        return rhs.getDate().compareTo(lhs.getDate());
      }
    };
  }
  // -------------------------------------------------------------------
  public static Map<String, Project> sortProjects(Map<String, Project> map,
                                                  AbstractProjectsComparator comparator) {
    final List<Entry<String, Project>> list = new LinkedList<>(map.entrySet());

    Collections.sort(list, comparator);

    final Map<String, Project> sorted = new LinkedHashMap<>();
    for (final Entry<String, Project> entry : list) {
      sorted.put(entry.getKey(), entry.getValue());
    }

    return sorted;
  }
  // _________________________________________________________________
  /**
   * Test program
   */
  public static void main(String[] args) {
    try {
      
      final GameLibrary          library  = new GameLibrary();
      // final StopWatch            sw       = new StopWatch();
      // sw.start();      
      final Map<String, Project> projects = library.getProjects(-1, 200);
      // sw.stop();
      
      for (final String key : projects.keySet()) {
        final Project project = projects.get(key);
        System.out.println(key + ": " + project); //NON-NLS
      }
      // System.out.println("Download took " + sw);

      final Console console = System.console();

      while (true) {
        final String slug = console.readLine("Select a project: "); //NON-NLS
        if (slug == null || slug.isBlank())
          break;
      
        final Project project = projects.get(slug);
        if (project == null)
          continue;
        
        library.getProject(slug, project);
        System.out.println(project);
      }
    }
    catch (Exception e) {
      System.out.println(e);
    }
  }
}
//
// EOF
//
