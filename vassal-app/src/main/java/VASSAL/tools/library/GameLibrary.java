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
package VASSAL.tools.library;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Reads a project from the VASSAL game library and downloads its files.
 *
 * <p>The library's REST API is the one the library website itself uses —
 * {@code https://vassalengine.org/api/gls/v1}. {@code GET /projects/{project}}
 * returns the whole project as JSON: packages, each with releases, each with
 * files carrying a direct download {@code url}, {@code size} and
 * {@code sha256}.</p>
 *
 * <p><b>Choosing "the latest" is per file, not per release.</b> A release is a
 * batch upload, so the same extension may appear in several: in the WiF
 * project {@code 23-DoD-III.vmdx} is in releases 2.1.3, 2.1.2 and 2.1.1, and
 * {@code 10-SiF.vmdx} in 2.1.2 and 2.1.1, while most extensions only ever
 * appeared in 2.1.1. Taking the newest release alone would fetch two
 * extensions and miss twenty-two; taking every release would fetch three
 * copies of some. So {@link Package#latestFiles()} groups by filename and
 * keeps the copy from the highest release version, comparing versions
 * numerically component-by-component and falling back to publication
 * time.</p>
 *
 * @since 3.8.0
 */
public final class GameLibrary {

  private static final Logger log = LoggerFactory.getLogger(GameLibrary.class);

  /** The public API the library website uses. */
  public static final String DEFAULT_API = "https://vassalengine.org/api/gls/v1"; //NON-NLS

  private static final int TIMEOUT_MS = 30_000;
  private static final String AGENT = "VASSAL"; //NON-NLS

  private final String apiBase;

  public GameLibrary(String apiBase) {
    this.apiBase = (apiBase == null || apiBase.trim().isEmpty()
      ? DEFAULT_API : apiBase.trim()).replaceAll("/+$", "");
  }

  // ---- project identity ------------------------------------------------

  /**
   * The project name from whatever the user pasted — a full page URL such as
   * {@code https://vassalengine.org/library/projects/Some_Project}, or the
   * bare name. Trailing slashes, query strings and fragments are ignored, so
   * a URL copied out of a browser works as-is.
   */
  public static String projectNameFrom(String input) {
    String s = input == null ? "" : input.trim();
    final int hash = s.indexOf('#');
    if (hash >= 0) {
      s = s.substring(0, hash);
    }
    final int q = s.indexOf('?');
    if (q >= 0) {
      s = s.substring(0, q);
    }
    s = s.replaceAll("/+$", "");
    final int slash = s.lastIndexOf('/');
    return slash >= 0 ? s.substring(slash + 1) : s;
  }

  // ---- model -------------------------------------------------------------

  /** One downloadable file, in the release that published it. */
  public static final class RemoteFile {
    public final String filename;
    public final String url;
    public final String sha256;
    public final String publishedAt;
    public final String releaseVersion;
    public final long size;

    RemoteFile(String filename, String url, long size, String sha256,
               String publishedAt, String releaseVersion) {
      this.filename = filename;
      this.url = url;
      this.size = size;
      this.sha256 = sha256;
      this.publishedAt = publishedAt;
      this.releaseVersion = releaseVersion;
    }

    public boolean isModule() {
      return hasSuffix(".vmod"); //NON-NLS
    }

    public boolean isExtension() {
      return hasSuffix(".vmdx"); //NON-NLS
    }

    public boolean isSavedGame() {
      return hasSuffix(".vsav"); //NON-NLS
    }

    private boolean hasSuffix(String suffix) {
      return filename != null
        && filename.toLowerCase(Locale.ROOT).endsWith(suffix);
    }

    /** {@code 10-SiF.vmdx} → {@code 10-SiF}, i.e. the extension name a save records. */
    public String extensionName() {
      final int dot = filename.lastIndexOf('.');
      return dot > 0 ? filename.substring(0, dot) : filename;
    }

    @Override
    public String toString() {
      return filename + " (" + releaseVersion + ')';
    }
  }

  /** A package: a named group of releases, e.g. "Extensions for …". */
  public static final class Package {
    public final String name;
    public final List<RemoteFile> files = new ArrayList<>();

    Package(String name) {
      this.name = name;
    }

    /**
     * One file per filename — the copy from the highest release version.
     * See the class notes for why this is per file rather than per release.
     */
    public List<RemoteFile> latestFiles() {
      final Map<String, RemoteFile> best = new LinkedHashMap<>();
      for (final RemoteFile f : files) {
        final RemoteFile cur = best.get(f.filename);
        if (cur == null || newer(f, cur)) {
          best.put(f.filename, f);
        }
      }
      final List<RemoteFile> out = new ArrayList<>(best.values());
      out.sort(Comparator.comparing(f -> f.filename == null ? "" : f.filename));
      return out;
    }

    public boolean hasExtensions() {
      for (final RemoteFile f : files) {
        if (f.isExtension()) {
          return true;
        }
      }
      return false;
    }

    @Override
    public String toString() {
      return name;
    }
  }

  /** A whole project. */
  public static final class Project {
    public final String name;
    public final List<Package> packages = new ArrayList<>();

    Project(String name) {
      this.name = name;
    }

    /** Packages containing at least one {@code .vmod}. */
    public List<Package> modulePackages() {
      final List<Package> out = new ArrayList<>();
      for (final Package p : packages) {
        for (final RemoteFile f : p.latestFiles()) {
          if (f.isModule()) {
            out.add(p);
            break;
          }
        }
      }
      return out;
    }

    /** Every extension in the project, latest copy of each, across packages. */
    public List<RemoteFile> latestExtensions() {
      final Map<String, RemoteFile> best = new LinkedHashMap<>();
      for (final Package p : packages) {
        for (final RemoteFile f : p.latestFiles()) {
          if (!f.isExtension()) {
            continue;
          }
          final RemoteFile cur = best.get(f.filename);
          if (cur == null || newer(f, cur)) {
            best.put(f.filename, f);
          }
        }
      }
      final List<RemoteFile> out = new ArrayList<>(best.values());
      out.sort(Comparator.comparing(f -> f.filename));
      return out;
    }
  }

  /** Sorts newest release first (see {@link #newer}) — for choosers. */
  public static void sortNewestFirst(List<RemoteFile> files) {
    files.sort((a, b) -> newer(a, b) ? -1 : (newer(b, a) ? 1 : 0));
  }

  /** Higher release version wins; publication time breaks ties. */
  static boolean newer(RemoteFile a, RemoteFile b) {
    final int c = compareVersions(a.releaseVersion, b.releaseVersion);
    if (c != 0) {
      return c > 0;
    }
    return a.publishedAt != null && b.publishedAt != null
      && a.publishedAt.compareTo(b.publishedAt) > 0;
  }

  /** Numeric component-wise comparison: {@code 2.1.10} sorts above {@code 2.1.9}. */
  static int compareVersions(String a, String b) {
    final String[] x = (a == null ? "" : a).split("[.\\-+]");
    final String[] y = (b == null ? "" : b).split("[.\\-+]");
    for (int i = 0; i < Math.max(x.length, y.length); i++) {
      final String xi = i < x.length ? x[i] : "";
      final String yi = i < y.length ? y[i] : "";
      final int c;
      if (xi.matches("\\d+") && yi.matches("\\d+")) {
        c = Long.compare(Long.parseLong(xi), Long.parseLong(yi));
      }
      else {
        c = xi.compareTo(yi);
      }
      if (c != 0) {
        return c;
      }
    }
    return 0;
  }

  // ---- fetching ------------------------------------------------------------

  /** {@code GET /projects/{name}} and parse it. */
  public Project fetchProject(String projectName) throws IOException {
    final String url = apiBase + "/projects/" + encode(projectName); //NON-NLS
    log.info("Fetching {}", url); //NON-NLS
    final String body = getText(url);
    final Object root;
    try {
      root = Json.parse(body);
    }
    catch (IllegalArgumentException e) {
      throw new IOException("the library returned something that is not JSON: " //NON-NLS
        + e.getMessage(), e);
    }
    final Project project = new Project(Json.str(Json.get(root, "name"), projectName)); //NON-NLS
    for (final Object po : Json.arr(Json.get(root, "packages"))) { //NON-NLS
      final Package pkg = new Package(Json.str(Json.get(po, "name"), "(unnamed)")); //NON-NLS
      for (final Object ro : Json.arr(Json.get(po, "releases"))) { //NON-NLS
        final String version = Json.str(Json.get(ro, "version"), ""); //NON-NLS
        for (final Object fo : Json.arr(Json.get(ro, "files"))) { //NON-NLS
          final String fn = Json.str(Json.get(fo, "filename"), null); //NON-NLS
          final String fu = Json.str(Json.get(fo, "url"), null); //NON-NLS
          if (fn == null || fu == null) {
            continue;
          }
          pkg.files.add(new RemoteFile(fn, fu,
            Json.num(Json.get(fo, "size"), -1), //NON-NLS
            Json.str(Json.get(fo, "sha256"), null), //NON-NLS
            Json.str(Json.get(fo, "published_at"), null), //NON-NLS
            version));
        }
      }
      project.packages.add(pkg);
    }
    return project;
  }

  private static String encode(String s) {
    // Project names are a single path segment, so '/' must go too. Everything
    // else is handled by encodeUrlPath() when the request is opened.
    return s.replace(" ", "%20").replace("#", "%23") //NON-NLS
            .replace("?", "%3F").replace("/", "%2F"); //NON-NLS
  }

  /** Characters legal unescaped in a URL path (RFC 3986 pchar, plus '/'). */
  private static final String PATH_SAFE =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" //NON-NLS
    + "-._~!$&'()*+,;=:@/";

  /**
   * Percent-encodes the path of {@code url}, leaving scheme and host alone.
   *
   * <p>Needed because the library's download URLs embed the filename
   * verbatim, and module filenames routinely contain spaces —
   * {@code .../WiF CE Official Combo ver 2_1_1.vmod}.
   * {@code HttpURLConnection} passes a raw space straight into the request
   * line, and the object store answers HTTP 400. Extensions downloaded fine
   * only because their filenames happen to have no spaces, which is what made
   * this look like a module-specific fault.</p>
   *
   * <p>An existing {@code %XX} escape is copied through untouched, so a URL
   * that is already encoded is left alone rather than double-encoded — that
   * would turn {@code %20} into {@code %2520} and 400 all over again.</p>
   */
  static String encodeUrlPath(String url) {
    final int scheme = url.indexOf("://"); //NON-NLS
    if (scheme < 0) {
      return url;
    }
    final int pathStart = url.indexOf('/', scheme + 3);
    if (pathStart < 0) {
      return url;
    }

    final String path = url.substring(pathStart);
    final StringBuilder sb = new StringBuilder(url.substring(0, pathStart));
    for (int i = 0; i < path.length(); i++) {
      final char c = path.charAt(i);
      if (c == '%' && i + 2 < path.length()
          && isHex(path.charAt(i + 1)) && isHex(path.charAt(i + 2))) {
        sb.append(path, i, i + 3);          // already escaped
        i += 2;
      }
      else if (PATH_SAFE.indexOf(c) >= 0) {
        sb.append(c);
      }
      else {
        for (final byte b : String.valueOf(c).getBytes(StandardCharsets.UTF_8)) {
          sb.append('%').append(String.format("%02X", b & 0xFF)); //NON-NLS
        }
      }
    }
    return sb.toString();
  }

  private static boolean isHex(char c) {
    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
  }

  private String getText(String url) throws IOException {
    final HttpURLConnection c = open(url, "application/json"); //NON-NLS
    try (InputStream in = c.getInputStream()) {
      final ByteArrayOutputStream buf = new ByteArrayOutputStream();
      copy(in, buf, null, 0, null, null);
      return new String(buf.toByteArray(), StandardCharsets.UTF_8);
    }
    finally {
      c.disconnect();
    }
  }

  private static HttpURLConnection open(String url, String accept) throws IOException {
    final HttpURLConnection c =
      (HttpURLConnection) new URL(encodeUrlPath(url)).openConnection();
    c.setConnectTimeout(TIMEOUT_MS);
    c.setReadTimeout(TIMEOUT_MS);
    c.setInstanceFollowRedirects(true);
    c.setRequestProperty("User-Agent", AGENT); //NON-NLS
    if (accept != null) {
      c.setRequestProperty("Accept", accept); //NON-NLS
    }
    final int code = c.getResponseCode();
    if (code == 404) {
      throw new IOException("not found (404): " + url); //NON-NLS
    }
    if (code / 100 != 2) {
      throw new IOException("HTTP " + code + " from " + url); //NON-NLS
    }
    return c;
  }

  /** Progress sink for a download; return false from {@link #onProgress} to cancel. */
  public interface Progress {
    boolean onProgress(String filename, long done, long total);
  }

  /**
   * Downloads one file into {@code dir}, verifying its SHA-256 when the
   * library supplied one. Writes to a temp file and moves it into place, so a
   * failed or cancelled download never leaves a truncated module behind.
   *
   * @return the file written, or {@code null} if cancelled
   */
  public File download(RemoteFile f, File dir, Progress progress)
                                                          throws IOException {
    if (!dir.isDirectory() && !dir.mkdirs()) {
      throw new IOException("cannot create " + dir); //NON-NLS
    }
    final File target = new File(dir, f.filename);
    final File tmp;
    try {
      tmp = File.createTempFile(f.filename + ".", ".part", dir); //NON-NLS
    }
    catch (IOException e) {
      // The JDK's own message here is a bare "Permission denied", naming
      // neither the file nor the folder — useless in a report from a user.
      throw new IOException("cannot write to " + dir + ": " + e.getMessage(), e); //NON-NLS
    }
    final HttpURLConnection c = open(f.url, null);
    boolean cancelled = false;
    try (InputStream in = c.getInputStream();
         OutputStream out = Files.newOutputStream(tmp.toPath())) {
      final MessageDigest digest = digest();
      cancelled = !copy(in, out, digest, f.size, f.filename, progress);
      if (!cancelled && f.sha256 != null && digest != null) {
        final String got = hex(digest.digest());
        if (!got.equalsIgnoreCase(f.sha256)) {
          throw new IOException(f.filename + ": SHA-256 mismatch — expected " //NON-NLS
            + f.sha256 + ", got " + got); //NON-NLS
        }
      }
    }
    finally {
      c.disconnect();
      if (cancelled) {
        Files.deleteIfExists(tmp.toPath());
      }
    }
    if (cancelled) {
      return null;
    }
    try {
      Files.move(tmp.toPath(), target.toPath(),
        StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
    }
    catch (IOException atomicUnsupported) {
      Files.move(tmp.toPath(), target.toPath(), StandardCopyOption.REPLACE_EXISTING);
    }
    return target;
  }

  private static MessageDigest digest() {
    try {
      return MessageDigest.getInstance("SHA-256"); //NON-NLS
    }
    catch (Exception e) {
      log.warn("SHA-256 unavailable; downloads will not be verified"); //NON-NLS
      return null;
    }
  }

  private static boolean copy(InputStream in, OutputStream out, MessageDigest digest,
                              long total, String name, Progress progress)
                                                          throws IOException {
    final byte[] buf = new byte[1 << 16];
    long done = 0;
    int n;
    while ((n = in.read(buf)) > 0) {
      out.write(buf, 0, n);
      if (digest != null) {
        digest.update(buf, 0, n);
      }
      done += n;
      if (progress != null && !progress.onProgress(name, done, total)) {
        return false;
      }
    }
    return true;
  }

  private static String hex(byte[] b) {
    final StringBuilder sb = new StringBuilder(b.length * 2);
    for (final byte x : b) {
      sb.append(String.format("%02x", x)); //NON-NLS
    }
    return sb.toString();
  }
}
