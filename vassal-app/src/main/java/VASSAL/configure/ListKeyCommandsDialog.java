package VASSAL.configure;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.KeyNamer;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.search.SearchTarget;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.swing.SwingUtils;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.StringUtils;

import java.awt.Frame;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
//import java.util.Objects;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RowFilter;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

public class ListKeyCommandsDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  public ListKeyCommandsDialog(Frame owner, List<String[]> rows) {
    super(owner, Resources.getString("Editor.ListKeyCommands.remove_unused_images"), true);

    final JTextField filter = new JTextField(25);

    final MyTableModel tmod = new MyTableModel(rows);
    final JTable table = new JTable(tmod);

    final TableRowSorter trs = new TableRowSorter(tmod);
    table.setRowSorter(trs);
    trs.setSortsOnUpdates(true);

    trs.setRowFilter(new RowFilter<TableModel, Integer>() {
      @Override
      public boolean include(Entry<? extends TableModel, ? extends Integer> entry) {
        // show row on an empty filter
        final String f = filter.getText();
        if (f == null) {
          return true;
        }

        // show row containing the filter as a substring
        for (int i = entry.getValueCount() - 1; i >= 0; i--) {
          final String v = entry.getStringValue(i);
          if (v != null && v.contains(f)) {
            return true;
          }
        }
        return false;
      }
    });

    final JPanel panel = new JPanel(new MigLayout("fill", "[]", "[]rel[]unrel[]"));  //NON-NLS

    panel.add(filter, "split");

    filter.getDocument().addDocumentListener(new DocumentListener() {
      @Override
      public void insertUpdate(DocumentEvent e) {
        update();
      }

      @Override
      public void removeUpdate(DocumentEvent e) {
        update();
      }

      @Override
      public void changedUpdate(DocumentEvent e) {
        update();
      }

      private void update() {
        trs.allRowsChanged();
      }
    });

    final NoInsetButton clear = new NoInsetButton("no", IconFamily.XSMALL, "Editor.clear");
    clear.addActionListener(e -> filter.setText(null));
    panel.add(clear, "wrap");


    final JScrollPane scroll = new JScrollPane(table);
    panel.add(scroll, "grow, push, wrap");

    final JButton ok = new JButton(Resources.getString("General.ok"));
    ok.addActionListener(e -> dispose());

    final JButton help = new JButton(Resources.getString("General.help"));
    help.addActionListener(e -> help());

    final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]push", "")); // NON-NLS
    buttonPanel.add(ok, "tag ok, sg 1"); //$NON-NLS-1$//
    buttonPanel.add(help, "tag help, sg 1");
    panel.add(buttonPanel, "growx"); // NON-NLS

    setLayout(new MigLayout("insets dialog, fill")); // NON-NLS
    add(panel, "grow"); // NON-NLS

    SwingUtils.repack(this);
  }

  private static class MyTableModel extends AbstractTableModel {
    private static final long serialVersionUID = 1L;

    private final List<String[]> rows;

    private final String[] columnNames = {
      "Key", "Name", "Source Name", "Source Description"
    };

    public MyTableModel(List<String[]> rows) {
      this.rows = rows;
    }

    @Override
    public int getRowCount() {
      return rows.size();
    }

    @Override
    public int getColumnCount() {
      return columnNames.length;
    }

    @Override
    public String getColumnName(int col) {
      return columnNames[col];
    }

    @Override
    public Object getValueAt(int row, int column) {
      return row < rows.size() ? rows.get(row)[column] : null;
    }
  }

  private static void checkSearchTarget(SearchTarget target, List<String[]> list) {
    final List<NamedKeyStroke> keys = target.getNamedKeyStrokeList();
    if (keys != null) {
      for (final NamedKeyStroke k : keys) {
        if (k != null) {
          String cmd_key = null;
          String cmd_name = null;

          if (k.isNamed()) {
            cmd_name = k.getName();
          }
          else {
            cmd_key = KeyNamer.getKeyString(k.getStroke());
          }

          if (!StringUtils.isEmpty(cmd_key) || !StringUtils.isEmpty(cmd_name)) { // Could check a filter here?
            String src_name = null;
            String src_desc = null;

            if (target instanceof AbstractConfigurable) {
              src_name = ((AbstractConfigurable)target).getConfigureName();
            }
            else if (target instanceof GamePiece) {
              src_name = ((GamePiece)target).getName();
              if (target instanceof Decorator) {
                src_desc = ((Decorator) target).getDescription();
              }
            }

            list.add(new String[] { cmd_key, cmd_name, src_name, src_desc });
          }
        }
      }
    }
  }

  private static void checkForKeyCommands(AbstractConfigurable target, List<String[]> list) {
    GamePiece p;
    boolean protoskip;
    if (target instanceof GamePiece) {
      p = (GamePiece) target;
      protoskip = false;
    }
    else if (target instanceof PieceSlot) {
      p = ((PieceSlot)target).getPiece();
      protoskip = false;
    }
    else if (target instanceof PrototypeDefinition) {
      p = ((PrototypeDefinition)target).getPiece();
      protoskip = true;
    }
    else  {
      checkSearchTarget(target, list);
      return;
    }

    // We're going to search Decorator from inner-to-outer (BasicPiece-on-out), so that user sees the traits hit in
    // the same order they're listed in the PieceDefiner window. So we first traverse them in the "normal" direction
    // outer to inner and make a list in the order we want to traverse it (for architectural reasons, just traversing
    // with getOuter() would take us inside of prototypes inside a piece, which we don't want).
    final List<GamePiece> pieces = new ArrayList<>();
    pieces.add(p);
    while (p instanceof Decorator) {
      p = ((Decorator) p).getInner();
      pieces.add(p);
    }
    Collections.reverse(pieces);

    for (final GamePiece piece : pieces) {
      if (!protoskip) { // Skip the fake "Basic Piece" on a Prototype definition
        if (piece instanceof SearchTarget) {
          checkSearchTarget((SearchTarget)piece, list);
        }
      }
      protoskip = false;
    }
  }

  private static void recursivelyFindKeyCommands(AbstractBuildable target, List<String[]> list) {
    for (final Buildable b : target.getBuildables()) {
      if (b instanceof AbstractConfigurable) {
        checkForKeyCommands((AbstractConfigurable)b, list);
      }

      if (b instanceof AbstractBuildable) {
        recursivelyFindKeyCommands((AbstractBuildable)b, list);
      }
    }
  }

  public static List<String[]> findAllKeyCommands() {
    final List<String[]> keyCommandList = new ArrayList<>();
    recursivelyFindKeyCommands(GameModule.getGameModule(), keyCommandList);
    return keyCommandList;
  }

  private void help() {
    HelpFile hf = null;
    try {
      hf = new HelpFile(null, new File(
        new File(Documentation.getDocumentationBaseDir(), "ReferenceManual"),
        "ListKeyCommands.html"));
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
    }

    (new ShowHelpAction(hf.getContents(), null)).actionPerformed(null);
  }
}
