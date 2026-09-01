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

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Window;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;
import javax.swing.WindowConstants;
import javax.swing.border.EmptyBorder;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.io.DeobfuscatingInputStream;

/**
 * Downloads a module and its extensions from the VASSAL game library.
 *
 * <p>The flow is a sequence of prompts rather than one big form, because each
 * step depends on the previous one: what the library actually offers decides
 * which module the user can pick, and the module's filename decides the name
 * of the extensions folder.</p>
 *
 * <ol>
 *   <li>the project — a library page URL or the bare project name;</li>
 *   <li>which module, if the project publishes more than one (the WiF project
 *       has both a 31 MB module and a 7 KB placeholder pointing at Google
 *       Drive, so picking automatically would be a coin toss);</li>
 *   <li>the folder to download into;</li>
 *   <li>optionally a saved game, to fetch only the extensions it names.</li>
 * </ol>
 *
 * <p>Extensions go in a sibling {@code <module>_ext} folder — the convention
 * VASSAL's ExtensionsManager expects — created on demand. Only the newest
 * copy of each extension is fetched; see
 * {@link GameLibrary.Package#latestFiles()} for why that is decided per file
 * rather than per release.</p>
 *
 * @since 3.8.0
 */
public final class DownloadModuleDialog {

  private static final Logger log = LoggerFactory.getLogger(DownloadModuleDialog.class);

  private final Window owner;

  public DownloadModuleDialog(Window owner) {
    this.owner = owner;
  }

  /**
   * Runs the whole flow.
   *
   * @param startDir the directory choosers start in; may be null
   * @return a short summary of what happened, or null if cancelled
   */
  public String run(File startDir) {
    final String input = (String) JOptionPane.showInputDialog(owner,
      "<html>Paste the library page for the module, or just its project name:<br>" //NON-NLS
      + "<tt>https://vassalengine.org/library/projects/<b>Project_Name</b></tt><br><br>" //NON-NLS
      + "If you do not know the module's URL, you can find it with the search<br>" //NON-NLS
      + "dialog on <tt>https://vassalengine.org/library/projects</tt></html>", //NON-NLS
      "Download Module from Library", JOptionPane.QUESTION_MESSAGE, null, null, ""); //NON-NLS
    if (input == null || input.trim().isEmpty()) {
      return null;
    }

    final String project = GameLibrary.projectNameFrom(input);
    final GameLibrary library = new GameLibrary(null);

    final GameLibrary.Project info;
    try {
      info = fetchWithProgress(library, project);
    }
    catch (Exception e) {
      JOptionPane.showMessageDialog(owner,
        "<html>Could not read <b>" + escape(project) + "</b> from the library:<br><br>" //NON-NLS
        + escape(String.valueOf(e.getMessage())) + "</html>", //NON-NLS
        "Library Unavailable", JOptionPane.ERROR_MESSAGE); //NON-NLS
      return "Library lookup failed for " + project + "."; //NON-NLS
    }
    if (info == null) {
      return null;              // cancelled
    }

    // --- which module -------------------------------------------------
    final List<GameLibrary.RemoteFile> modules = new ArrayList<>();
    for (final GameLibrary.Package p : info.modulePackages()) {
      for (final GameLibrary.RemoteFile f : p.latestFiles()) {
        if (f.isModule()) {
          modules.add(f);
        }
      }
    }
    if (modules.isEmpty()) {
      JOptionPane.showMessageDialog(owner,
        "<html><b>" + escape(project) + "</b> publishes no <tt>.vmod</tt> file.</html>", //NON-NLS
        "No Module Found", JOptionPane.WARNING_MESSAGE); //NON-NLS
      return "No module published by " + project + "."; //NON-NLS
    }
    final GameLibrary.RemoteFile module;
    if (modules.size() == 1) {
      module = modules.get(0);
    }
    else {
      // most recent release at the top of the list
      GameLibrary.sortNewestFirst(modules);
      final Object[] choices = modules.toArray();
      final Object picked = JOptionPane.showInputDialog(owner,
        "This project publishes more than one module. Which one?", //NON-NLS
        "Choose Module", JOptionPane.QUESTION_MESSAGE, null, choices, choices[0]); //NON-NLS
      if (picked == null) {
        return null;
      }
      module = (GameLibrary.RemoteFile) picked;
    }

    // --- where to put it ----------------------------------------------
    // Asked in a loop: a folder that cannot be written to is the commonest
    // way for this to fail, and the user should be able to pick another
    // rather than watch every download fail in turn.
    File chosen = null;
    File start = startDir;
    while (chosen == null) {
      final JFileChooser fc = new JFileChooser();
      fc.setDialogTitle("Choose the folder to download into"); //NON-NLS
      fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      if (start != null && start.isDirectory()) {
        fc.setCurrentDirectory(start);
      }
      if (fc.showSaveDialog(owner) != JFileChooser.APPROVE_OPTION) {
        return null;
      }
      final File candidate = fc.getSelectedFile();
      final String problem = writeProblem(candidate);
      if (problem == null) {
        chosen = candidate;
        break;
      }
      log.warn("Download folder {} is not writable: {}", candidate, problem); //NON-NLS
      final int again = JOptionPane.showConfirmDialog(owner,
        "<html>Nothing can be written into<br><tt>" //NON-NLS
        + escape(candidate.getAbsolutePath()) + "</tt><br><br><tt>" //NON-NLS
        + escape(problem) + "</tt><br><br>" //NON-NLS
        + "A folder inside <b>Program Files</b> (or any other protected " //NON-NLS
        + "location) needs administrator rights, which VASSAL does " //NON-NLS
        + "not run with. Antivirus \"controlled folder access\" can do the " //NON-NLS
        + "same thing.<br><br>Choose a folder you own \u2014 somewhere under your " //NON-NLS
        + "user folder, such as <tt>Documents</tt>.<br><br>Pick a different " //NON-NLS
        + "folder?</html>", //NON-NLS
        "Folder Not Writable", JOptionPane.OK_CANCEL_OPTION, //NON-NLS
        JOptionPane.ERROR_MESSAGE);
      if (again != JOptionPane.OK_OPTION) {
        return "Download cancelled: " + candidate + " is not writable."; //NON-NLS
      }
      start = candidate.getParentFile();
    }
    final File dir = chosen;

    // --- optional scenario filter -------------------------------------
    List<GameLibrary.RemoteFile> extensions = info.latestExtensions();
    final Set<String> missingFromLibrary = new LinkedHashSet<>();
    String scenarioNote = "";
    if (!extensions.isEmpty()) {
      final int scope = JOptionPane.showConfirmDialog(owner,
        "<html>This project has <b>" + extensions.size() //NON-NLS
        + "</b> extension(s).<br><br>Download <b>all</b> of them?<br><br>" //NON-NLS
        + "Choose <b>No</b> to pick a saved game and fetch only the extensions " //NON-NLS
        + "it needs.</html>", //NON-NLS
        "Extensions", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE); //NON-NLS
      if (scope == JOptionPane.CANCEL_OPTION || scope == JOptionPane.CLOSED_OPTION) {
        return null;
      }
      if (scope == JOptionPane.NO_OPTION) {
        final JFileChooser sc = new JFileChooser();
        sc.setDialogTitle("Choose a saved game"); //NON-NLS
        sc.setFileFilter(new FileNameExtensionFilter(
          "VASSAL Saved Games (*.vsav)", "vsav")); //NON-NLS
        if (startDir != null && startDir.isDirectory()) {
          sc.setCurrentDirectory(startDir);
        }
        if (sc.showOpenDialog(owner) != JFileChooser.APPROVE_OPTION) {
          return null;
        }
        final Set<String> needed;
        try {
          needed = scenarioExtensionNames(sc.getSelectedFile());
        }
        catch (Exception e) {
          JOptionPane.showMessageDialog(owner,
            "Could not read that saved game: " + e.getMessage(), //NON-NLS
            "Unreadable Saved Game", JOptionPane.ERROR_MESSAGE); //NON-NLS
          return null;
        }
        final List<GameLibrary.RemoteFile> filtered = new ArrayList<>();
        final Set<String> available = new LinkedHashSet<>();
        for (final GameLibrary.RemoteFile f : extensions) {
          available.add(f.extensionName());
          if (needed.contains(f.extensionName())) {
            filtered.add(f);
          }
        }
        for (final String n : needed) {
          if (!available.contains(n)) {
            missingFromLibrary.add(n);
          }
        }
        extensions = filtered;
        scenarioNote = " for the chosen scenario"; //NON-NLS
        if (!missingFromLibrary.isEmpty()) {
          final int go = JOptionPane.showConfirmDialog(owner,
            "<html>The scenario names <b>" + missingFromLibrary.size() //NON-NLS
            + "</b> extension(s) the library does not publish:<br><br><tt>" //NON-NLS
            + escape(String.join("<br>", missingFromLibrary)) //NON-NLS
            + "</tt><br><br>Continue with the " //NON-NLS
            + extensions.size() + " that are available?</html>", //NON-NLS
            "Some Extensions Unavailable", //NON-NLS
            JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
          if (go != JOptionPane.OK_OPTION) {
            return null;
          }
        }
      }
    }

    // --- confirm and go -----------------------------------------------
    final File extDir = extensionsDirFor(dir, module.filename);
    final StringBuilder ask = new StringBuilder("<html>Download into <tt>") //NON-NLS
      .append(escape(dir.getAbsolutePath())).append("</tt>:<br><br><b>") //NON-NLS
      .append(escape(module.filename)).append("</b> (") //NON-NLS
      .append(mb(module.size)).append(")");
    if (!extensions.isEmpty()) {
      long total = 0;
      for (final GameLibrary.RemoteFile f : extensions) {
        total += Math.max(f.size, 0);
      }
      ask.append("<br>and <b>").append(extensions.size()).append("</b> extension(s)") //NON-NLS
         .append(scenarioNote).append(" (").append(mb(total)).append(") into <tt>") //NON-NLS
         .append(escape(extDir.getName())).append("/</tt>"); //NON-NLS
    }
    ask.append("</html>"); //NON-NLS
    if (JOptionPane.showConfirmDialog(owner, ask.toString(), "Confirm Download", //NON-NLS
        JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE)
        != JOptionPane.OK_OPTION) {
      return null;
    }
    return downloadAll(library, module, extensions, dir, extDir, missingFromLibrary);
  }

  /**
   * The extension names a saved game records — the first field of each of its
   * {@code EXT\t<name>\t<version>} commands.
   *
   * <p>The command log is scanned token by token and only {@code EXT}
   * commands are kept, so memory stays bounded no matter how large the save
   * is.</p>
   */
  static Set<String> scenarioExtensionNames(File vsav) throws IOException {
    final Set<String> names = new LinkedHashSet<>();
    try (ZipFile zf = new ZipFile(vsav)) {
      final ZipEntry entry = zf.getEntry("savedGame"); //NON-NLS
      if (entry == null) {
        throw new IOException(vsav.getName() + " is not a VASSAL saved game"); //NON-NLS
      }
      try (InputStream in = new BufferedInputStream(new DeobfuscatingInputStream(
             new BufferedInputStream(zf.getInputStream(entry))))) {
        final byte[] prefix = {'E', 'X', 'T', '\t'};
        final ByteArrayOutputStream tok = new ByteArrayOutputStream();
        boolean keeping = true;
        int b;
        while ((b = in.read()) >= 0) {
          if (b == 0x1B) {        // command separator
            addExtensionName(tok, keeping, names);
            tok.reset();
            keeping = true;
          }
          else if (keeping) {
            final int len = tok.size();
            if (len < prefix.length && (byte) b != prefix[len]) {
              keeping = false;    // not an EXT command; skip to the next one
              tok.reset();
            }
            else if (len < 4096) {
              tok.write(b);
            }
          }
        }
        addExtensionName(tok, keeping, names);
      }
    }
    return names;
  }

  private static void addExtensionName(ByteArrayOutputStream tok, boolean keeping,
                                       Set<String> names) {
    if (!keeping || tok.size() <= 4) {
      return;
    }
    final String content = new String(tok.toByteArray(), StandardCharsets.UTF_8);
    final String name =
      new SequenceEncoder.Decoder(content.substring(4), '\t').nextToken("");
    if (!name.isEmpty()) {
      names.add(name);
    }
  }

  /**
   * Whether files can actually be written under {@code dir}.
   *
   * <p>By writing a file, not by asking. {@code File.canWrite()} reports the
   * read-only <em>attribute</em> on Windows and ignores ACLs, so it answers
   * "yes" for {@code C:\Program Files} — where an unelevated process cannot
   * create anything. The only trustworthy test is to create a file and remove
   * it again.</p>
   *
   * <p>Probes the nearest existing ancestor when {@code dir} does not exist
   * yet, so a folder the user may not go on to confirm is never created.
   * Write permission on that ancestor is what creating {@code dir} needs
   * anyway.</p>
   *
   * @return null when writable, else the reason it is not
   */
  static String writeProblem(File dir) {
    File probeIn = dir;
    while (probeIn != null && !probeIn.isDirectory()) {
      probeIn = probeIn.getParentFile();
    }
    if (probeIn == null) {
      return "no such folder: " + dir; //NON-NLS
    }
    try {
      final File probe = File.createTempFile("vassal-write-probe", ".tmp", probeIn); //NON-NLS
      Files.delete(probe.toPath());
      return null;
    }
    catch (IOException e) {
      final String why = e.getMessage() == null ? e.toString() : e.getMessage();
      return probeIn.equals(dir) ? why : why + " (in " + probeIn + ")"; //NON-NLS
    }
  }

  /** {@code Foo.vmod} → {@code <dir>/Foo_ext}, the convention VASSAL expects. */
  static File extensionsDirFor(File dir, String moduleFilename) {
    String stem = moduleFilename;
    if (stem.toLowerCase(Locale.ROOT).endsWith(".vmod")) { //NON-NLS
      stem = stem.substring(0, stem.length() - ".vmod".length()); //NON-NLS
    }
    return new File(dir, stem + "_ext"); //NON-NLS
  }

  private GameLibrary.Project fetchWithProgress(GameLibrary library, String project)
                                                          throws Exception {
    final JDialog d = busy("Reading <b>" + escape(project) + "</b> from the library\u2026"); //NON-NLS
    final SwingWorker<GameLibrary.Project, Void> w =
        new SwingWorker<>() {
          @Override
          protected GameLibrary.Project doInBackground() throws Exception {
            return library.fetchProject(project);
          }

          @Override
          protected void done() {
            d.dispose();
          }
        };
    w.execute();
    d.setVisible(true);
    return w.get();
  }

  private String downloadAll(GameLibrary library, GameLibrary.RemoteFile module,
                             List<GameLibrary.RemoteFile> extensions,
                             File dir, File extDir, Set<String> unavailable) {
    final List<GameLibrary.RemoteFile> queue = new ArrayList<>();
    queue.add(module);
    queue.addAll(extensions);

    final JDialog d = new JDialog(owner, "Downloading", Dialog.ModalityType.APPLICATION_MODAL); //NON-NLS
    d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    final JProgressBar bar = new JProgressBar(0, 100);
    bar.setStringPainted(true);
    final JTextArea logArea = new JTextArea(12, 66);
    logArea.setEditable(false);
    logArea.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
    final JButton stop = new JButton("Stop"); //NON-NLS
    final JPanel south = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    south.add(stop);
    final JPanel content = new JPanel(new BorderLayout(8, 8));
    content.setBorder(new EmptyBorder(12, 12, 12, 12));
    content.add(bar, BorderLayout.NORTH);
    content.add(new JScrollPane(logArea), BorderLayout.CENTER);
    content.add(south, BorderLayout.SOUTH);
    d.setContentPane(content);
    d.pack();
    d.setLocationRelativeTo(owner);

    final int[] okFailed = {0, 0};
    final List<String> failures = new ArrayList<>();
    log.info("Downloading {} file(s) into {} (extensions into {})", //NON-NLS
      queue.size(), dir, extDir);
    final SwingWorker<Void, String> worker = new SwingWorker<>() {
      @Override
      protected Void doInBackground() {
        int n = 0;
        for (final GameLibrary.RemoteFile f : queue) {
          if (isCancelled()) {
            break;
          }
          n++;
          final File into = f.isExtension() ? extDir : dir;
          final int index = n;
          publish("[" + index + "/" + queue.size() + "] " + f.filename //NON-NLS
            + "  (" + mb(f.size) + ", release " + f.releaseVersion + ")\n"); //NON-NLS
          try {
            final File got = library.download(f, into, (name, done, total) -> {
              if (isCancelled()) {
                return false;
              }
              if (total > 0) {
                setProgress((int) Math.min(100, done * 100 / total));
              }
              return true;
            });
            if (got == null) {
              publish("    cancelled\n"); //NON-NLS
              break;
            }
            okFailed[0]++;
          }
          catch (Exception e) {
            okFailed[1]++;
            final String why = e.getMessage() == null
              ? e.toString() : e.getMessage();
            failures.add(f.filename + ": " + why); //NON-NLS
            log.warn("Download failed: {} into {}", f.filename, into, e); //NON-NLS
            publish("    FAILED: " + why + "\n"); //NON-NLS
          }
        }
        return null;
      }

      @Override
      protected void process(List<String> chunks) {
        for (final String c : chunks) {
          logArea.append(c);
        }
        logArea.setCaretPosition(logArea.getDocument().getLength());
      }

      @Override
      protected void done() {
        d.dispose();
        // A SwingWorker keeps what its background half threw until asked.
        // Nobody asked, so anything escaping the per-file catch above was
        // discarded and the run looked like it had simply found nothing.
        if (isCancelled()) {
          return;
        }
        try {
          get();
        }
        catch (ExecutionException e) {
          final Throwable cause = e.getCause() == null ? e : e.getCause();
          log.error("Download run failed", cause); //NON-NLS
          failures.add("the download stopped: " + cause); //NON-NLS
          okFailed[1]++;
        }
        catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        }
      }
    };
    worker.addPropertyChangeListener(ev -> {
      if ("progress".equals(ev.getPropertyName())) { //NON-NLS
        bar.setValue((Integer) ev.getNewValue());
      }
    });
    stop.addActionListener(e -> worker.cancel(true));
    worker.execute();
    d.setVisible(true);

    final StringBuilder done = new StringBuilder("<html>Downloaded <b>") //NON-NLS
      .append(okFailed[0]).append("</b> file(s) into <tt>") //NON-NLS
      .append(escape(dir.getAbsolutePath())).append("</tt>."); //NON-NLS
    if (okFailed[1] > 0) {
      // Say why, not just how many: the reasons were previously written only
      // into the progress dialog, which is gone by the time this is read.
      done.append("<br><b>").append(okFailed[1]).append("</b> failed:<br><tt>"); //NON-NLS
      final int shown = Math.min(failures.size(), 3);
      for (int i = 0; i < shown; i++) {
        done.append(escape(failures.get(i))).append("<br>"); //NON-NLS
      }
      if (failures.size() > shown) {
        done.append("\u2026 and ").append(failures.size() - shown).append(" more<br>"); //NON-NLS
      }
      done.append("</tt>Full details are in the error log:<br><tt>") //NON-NLS
        .append(escape(Info.getErrorLogPath().getPath()))
        .append("</tt>"); //NON-NLS
    }
    if (!unavailable.isEmpty()) {
      done.append("<br><br>Not published by the library: <tt>") //NON-NLS
        .append(escape(String.join(", ", unavailable))).append("</tt>"); //NON-NLS
    }
    done.append("</html>"); //NON-NLS
    JOptionPane.showMessageDialog(owner, done.toString(), "Download Complete", //NON-NLS
      okFailed[1] > 0 ? JOptionPane.WARNING_MESSAGE : JOptionPane.INFORMATION_MESSAGE);
    return "Downloaded " + okFailed[0] + " file(s)" //NON-NLS
      + (okFailed[1] > 0 ? ", " + okFailed[1] + " failed" : "") + "."; //NON-NLS
  }

  private JDialog busy(String html) {
    final JDialog d = new JDialog(owner, "Working\u2026", Dialog.ModalityType.APPLICATION_MODAL); //NON-NLS
    d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    final JPanel p = new JPanel(new BorderLayout(10, 10));
    p.setBorder(new EmptyBorder(16, 20, 16, 20));
    p.add(new JLabel("<html>" + html + "</html>"), BorderLayout.NORTH); //NON-NLS
    final JProgressBar bar = new JProgressBar();
    bar.setIndeterminate(true);
    p.add(bar, BorderLayout.CENTER);
    d.setContentPane(p);
    d.pack();
    d.setMinimumSize(new Dimension(380, d.getHeight()));
    d.setLocationRelativeTo(owner);
    return d;
  }

  private static String mb(long bytes) {
    if (bytes < 0) {
      return "size unknown"; //NON-NLS
    }
    return bytes < 1024 * 1024
      ? String.format("%.0f KB", bytes / 1024.0) //NON-NLS
      : String.format("%.1f MB", bytes / (1024.0 * 1024.0)); //NON-NLS
  }

  private static String escape(String s) {
    return s == null ? "" : s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;"); //NON-NLS
  }
}
