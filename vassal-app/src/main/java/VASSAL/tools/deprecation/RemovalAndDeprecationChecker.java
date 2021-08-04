package VASSAL.tools.deprecation;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.apache.commons.lang3.tuple.Pair;

public class RemovalAndDeprecationChecker {

  private static final Set<String> REMOVED = Set.of(
    // methods
    "VASSAL.Info.compareVersions(java.lang.String, java.lang.String)",
    "VASSAL.Info.getDocsDir()",
    "VASSAL.Info.getMinorVersion()",
    "VASSAL.Info.is2dEnabled()",
    "VASSAL.Info.isDndEnabled()",
    "VASSAL.Info.isMacOsX()",
    "VASSAL.Info.isMacOSX()",
    "VASSAL.Info.isWindows()",
    "VASSAL.build.AbstractBuildable.getAllDescendantComponents(java.lang.Class)",
    "VASSAL.build.AbstractBuildable.getBuildComponents()",
    "VASSAL.build.AbstractBuildable.getComponents(java.lang.Class)",
    "VASSAL.build.GameModule.compareVersions(java.lang.String, java.lang.String)",
    "VASSAL.build.GameModule.fireKeyStroke(javax.swing.KeyStroke)",
    "VASSAL.build.GameModule.getFileDialog()",
    "VASSAL.build.GameModule.getFrame()",
    "VASSAL.build.GameModule.getGlobalPrefs()",
    "VASSAL.build.GameModule.setGlobalPrefs(VASSAL.preferences.Prefs)",
    "VASSAL.build.module.Chatter.getHandle()",
    "VASSAL.build.module.Chatter.setHandle(java.lang.String)",
    "VASSAL.build.module.DiceButton.getReportPrefix()",
    "VASSAL.build.module.DiceButton.getReportSuffix()",
    "VASSAL.build.module.EventLog.decodeEvents(java.lang.String)",
    "VASSAL.build.module.EventLog.encodeEvents(java.util.Enumeration)",
    "VASSAL.build.module.GameState.getGameComponentsEnum()",
    "VASSAL.build.module.GameState.getPieces()",
    "VASSAL.build.module.GlobalOptions.isAveragedScaling()",
    "VASSAL.build.module.Map.componentCoordinates(java.awt.Point)",
    "VASSAL.build.module.Map.componentRectangle(java.awt.Rectangle)",
    "VASSAL.build.module.Map.getAllBoards()",
    "VASSAL.build.module.Map.getAllMaps()",
    "VASSAL.build.module.Map.mapCoordinates(java.awt.Point)",
    "VASSAL.build.module.Map.mapRectangle(java.awt.Rectangle)",
    "VASSAL.build.module.Map.reposition(VASSAL.counters.GamePiece, int)",
    "VASSAL.build.module.Map.setBoards(java.util.Enumeration)",
    "VASSAL.build.module.ObscurableOptions$SetAllowed.ObscurableOptions$SetAllowed(java.util.Vector)",
    "VASSAL.build.module.PlayerRoster.addSideChangeListener(VASSAL.build.module.PlayerRoster.SideChangeListener)",
    "VASSAL.build.module.PlayerRoster.removeSideChangeListener(VASSAL.build.module.PlayerRoster.SideChangeListener)",
    "VASSAL.build.module.PrivateMap.setBoards(java.util.Enumeration)",
    "VASSAL.build.module.SpecialDiceButton.getReportSuffix()",
    "VASSAL.build.module.dice.DieServer.getReportSuffix()",
    "VASSAL.build.module.documentation.AboutScreen.AboutScreen(java.awt.Image)",
    "VASSAL.build.module.documentation.HelpFile.toURL(java.io.File)",
    "VASSAL.build.module.gamepieceimage.InstanceConfigurer.getValueArrayList()",
    "VASSAL.build.module.map.BoardPicker$SetBoards.BoardPicker$SetBoards(VASSAL.build.module.map.BoardPicker, java.util.Vector)",
    "VASSAL.build.module.map.BoardPicker.getCurrentBoards()",
    "VASSAL.build.module.map.BoardPicker.pack()",
    "VASSAL.build.module.map.BoardPicker.pickBoards()",
    "VASSAL.build.module.map.BoardPicker.setBoards(java.util.Enumeration)",
    "VASSAL.build.module.map.CounterDetailViewer.drawGraphics(java.awt.Graphics, java.awt.Point, javax.swing.JComponent, VASSAL.counters.PieceIterator)",
    "VASSAL.build.module.map.CounterDetailViewer.drawLabel(java.awt.Graphics, java.awt.Point, java.lang.String)",
    "VASSAL.build.module.map.CounterDetailViewer.drawText(java.awt.Graphics, java.awt.Point, javax.swing.JComponent, VASSAL.counters.PieceIterator)",
    "VASSAL.build.module.map.CounterDetailViewer$Visitor.CounterDetailViewer$Visitor(VASSAL.build.module.map.CounterDetailViewer$Filter, VASSAL.build.module.Map, java.awt.Point)", 
    "VASSAL.build.module.map.GlobalMap.componentCoordinates(java.awt.Point)",
    "VASSAL.build.module.map.GlobalMap.mapCoordinates(java.awt.Point)",
    "VASSAL.build.module.map.ImageSaver.writeImage(java.io.OutputStream[])",
    "VASSAL.build.module.map.MapShader.buildTexture()",
    "VASSAL.build.module.map.MapShader.getPatternRect()",
    "VASSAL.build.module.map.MapShader.getShadePattern()",
    "VASSAL.build.module.map.MapShader.getTexture()",
    "VASSAL.build.module.map.PieceMover.applyKeyAfterMove(java.util.List, VASSAL.command.Command, javax.swing.KeyStroke)",
    "VASSAL.build.module.map.PieceMover.selectMovablePieces(java.awt.Point)",
    "VASSAL.build.module.map.PieceMover.setOldLocation(VASSAL.counters.GamePiece)",
    "VASSAL.build.module.map.boardPicker.Board.cleanUp()",
    "VASSAL.build.module.map.boardPicker.Board.fixBounds()",
    "VASSAL.build.module.map.boardPicker.Board.fixImage()",
    "VASSAL.build.module.map.boardPicker.Board.fixImage(java.awt.Component)",
    "VASSAL.build.module.map.boardPicker.Board.getScaledImage(double, java.awt.Component)",
    "VASSAL.build.module.properties.EnumeratedPropertyPrompt.EnumeratedPropertyPrompt(VASSAL.build.module.properties.PropertyPrompt$DialogParent, java.lang.String, java.lang.String[])",
    "VASSAL.build.module.turn.TurnTracker.replace(java.lang.StringBuffer, java.lang.String, java.lang.String)",
    "VASSAL.chat.PrivateChatter.getHandle()",
    "VASSAL.chat.SimpleRoom.getPlayers()",
    "VASSAL.command.ChangePiece.ChangePiece(java.lang.String, java.lang.String)",
    "VASSAL.command.Command.hasNullSubcommands()",
    "VASSAL.command.ConditionalCommand$Eq.getValues()",
    "VASSAL.configure.ConfigureTree.buildAddActions(VASSAL.build.Configurable)",
    "VASSAL.configure.PieceAccessConfigurer.setPrompts(java.lang.String[])",
    "VASSAL.counters.Deck.Deck(java.lang.String)",
    "VASSAL.counters.Deck.Deck(java.lang.String, VASSAL.build.module.properties.PropertySource)",
    "VASSAL.counters.Deck.getEmptyKey()",
    "VASSAL.counters.Deck.setContents(java.util.Iterator)",
    "VASSAL.counters.Deck.setEmptyKey(javax.swing.KeyStroke)",
    "VASSAL.counters.Decorator.setOldProperties()",
    "VASSAL.counters.DragBuffer.init(VASSAL.counters.DragBuffer)",
    "VASSAL.counters.DragBuffer.sort(VASSAL.tools.Sort$Comparator)",
    "VASSAL.counters.Embellishment.getCurrentImage()",
    "VASSAL.counters.Embellishment.oldGetType()",
    "VASSAL.counters.Embellishment0$Ed.oldgetType()",
    "VASSAL.counters.Embellishment0.getCurrentImage()",
    "VASSAL.counters.Embellishment0.oldGetType()",
    "VASSAL.counters.Embellishment0.oldgetType()",
    "VASSAL.counters.Footprint.getPointList()",
    "VASSAL.counters.FreeRotator.getRotatedBounds()",
    "VASSAL.counters.FreeRotator.getRotatedImage(double, java.awt.Component)",
    "VASSAL.counters.Obscurable.isMaskableBy(java.lang.String)",
    "VASSAL.counters.Obscurable.setAllHidden(boolean)",
    "VASSAL.counters.PieceIterator.PieceIterator(java.util.Enumeration)",
    "VASSAL.counters.Stack.getPieces()",
    "VASSAL.counters.Stack.getPiecesInReverseOrder()",
    "VASSAL.counters.Stack.getPiecesInVisibleOrder()",
    "VASSAL.i18n.Resources.getEditorString(java.lang.String)",
    "VASSAL.i18n.Resources.getString(java.util.ResourceBundle, java.lang.String)",
    "VASSAL.i18n.Resources.getVassalString(java.lang.String)",
    "VASSAL.launch.PlayerWindow.splitControlPanel(java.awt.Component, int, boolean)",
    "VASSAL.tools.ArchiveWriter.ArchiveWriter(java.util.zip.ZipFile)",
    "VASSAL.tools.ArchiveWriter.isImageAdded(java.lang.String)",
    "VASSAL.tools.ArchiveWriter.write()",
    "VASSAL.tools.ArchiveWriter.write(boolean)",
    "VASSAL.tools.DataArchive.addImageSource(java.lang.String, VASSAL.tools.ImageSource)",
    "VASSAL.tools.DataArchive.clearScaledImageCache()",
    "VASSAL.tools.DataArchive.clearTransformedImageCache()",
    "VASSAL.tools.DataArchive.findImage(java.io.File, java.lang.String)",
    "VASSAL.tools.DataArchive.findImage(java.io.File, java.lang.String, java.lang.String)",
    "VASSAL.tools.DataArchive.getArchiveURL()",
    "VASSAL.tools.DataArchive.getBytes(java.io.InputStream)",
    "VASSAL.tools.DataArchive.getCachedImage(java.lang.String)",
    "VASSAL.tools.DataArchive.getFileStream(java.io.File, java.lang.String)",
    "VASSAL.tools.DataArchive.getFileStream(java.io.File, java.lang.String, java.lang.String)",
    "VASSAL.tools.DataArchive.getFileStream(java.lang.String)",
    "VASSAL.tools.DataArchive.getImageBounds(java.awt.Image)",
    "VASSAL.tools.DataArchive.getImageInputStream(java.lang.String)",
    "VASSAL.tools.DataArchive.getImage(java.io.InputStream)",
    "VASSAL.tools.DataArchive.getImage(java.lang.String)",
    "VASSAL.tools.DataArchive.getImageSize(java.lang.String)",
    "VASSAL.tools.DataArchive.getImageURL(java.lang.String)",
    "VASSAL.tools.DataArchive.getScaledImage(java.awt.Image, double)",
    "VASSAL.tools.DataArchive.getScaledImage(java.awt.Image, double, boolean, boolean)",
    "VASSAL.tools.DataArchive.getTransformedImage(java.awt.Image, double, double)",
    "VASSAL.tools.DataArchive.getTransformedImage(java.awt.Image, double, double, boolean)",
    "VASSAL.tools.DataArchive.improvedScaling(java.awt.Image, int, int)",
    "VASSAL.tools.DataArchive.isNameCacheStale()",
    "VASSAL.tools.DataArchive.listImageNames(java.util.Collection)",
    "VASSAL.tools.DataArchive.removeImageSource(java.lang.String)",
    "VASSAL.tools.DataArchive.setOfImageNames()",
    "VASSAL.tools.DataArchive.unCacheImage(java.awt.Image)",
    "VASSAL.tools.DataArchive.unCacheImage(java.lang.String)",
    "VASSAL.tools.JarArchive.getFileStream(java.lang.String)",
    "VASSAL.tools.image.ImageUtils.getImage(java.io.InputStream)",
    "VASSAL.tools.image.ImageUtils.getImageSize(java.io.InputStream)",
    "VASSAL.tools.image.ImageUtils.setHighQualityScaling(boolean)", 
    "VASSAL.tools.image.ImageUtils.transform(java.awt.image.BufferedImage, double, double, java.awt.RenderingHints, int)",
    "VASSAL.tools.menu.MenuScroller.dispose()",
    // members
    "VASSAL.build.module.Chatter.GAME_MSG_COLOR",
    "VASSAL.build.module.gamepieceimage.ImageItem.image",
    "VASSAL.build.module.map.BoardPicker.status",
    "VASSAL.build.module.map.CounterDetailViewer.ALWAYS_SHOW_LOC",
    "VASSAL.build.module.map.CounterDetailViewer.alwaysShowLoc",
    "VASSAL.build.module.map.SelectionHighlighter.image",
    "VASSAL.build.module.map.Zoomer.maxZoom",
    "VASSAL.build.module.map.Zoomer.zoom",
    "VASSAL.build.module.map.Zoomer.zoomFactor",
    "VASSAL.build.module.map.Zoomer.zoomLevel",
    "VASSAL.build.module.map.Zoomer.zoomStart",
    "VASSAL.build.module.map.boardPicker.Board.boardImage",
    "VASSAL.build.module.map.boardPicker.Board.boardName",
    "VASSAL.counters.FreeRotator.images",
    "VASSAL.counters.FreeRotator.unrotated",
    "VASSAL.tools.DataArchive.imageNames",
    "VASSAL.tools.DataArchive.imageSources",
    "VASSAL.tools.DataArchive.soundsDir",
    "VASSAL.tools.DataArchive.SOUNDS_DIR",
    // classes
    "VASSAL.build.module.documentation.HelpWindowExtension",
    "VASSAL.build.module.map.boardPicker.Board$Cleanup",
    "VASSAL.configure.ModuleUpdaterDialog",
    "VASSAL.tools.ZipUpdater",
    "VASSAL.tools.bug.Bug2694Handler"
  );

  private static final Set<String> DEPRECATED = Set.of(
    // members, methods
    "VASSAL.command.ConditionalCommand$Eq.ConditionalCommand$Eq(java.lang.String, java.util.Vector)",
    "VASSAL.i18n.ComponentI18nData.ComponentI18nData(VASSAL.build.AbstractConfigurable, java.lang.String, java.util.ArrayList, java.util.ArrayList, java.util.ArrayList)",
    "VASSAL.launch.TilingHandler.pid",
    "VASSAL.launch.TilingHandler.TilingHandler(java.lang.String, java.io.File, java.awt.Dimension, int, int)",
    "VASSAL.launch.ModuleManager.INITIAL_HEAP",
    "VASSAL.configure.StringArrayConfigurer.list",
    "VASSAL.configure.StringArrayConfigurer.model",
    "VASSAL.configure.StringArrayConfigurer.textField",
    "VASSAL.configure.StringArrayConfigurer.minRows",
    "VASSAL.configure.StringArrayConfigurer.maxRows",
    "VASSAL.configure.StringArrayConfigurer.getModel()",
    "VASSAL.configure.StringArrayConfigurer.addValue(java.lang.String)",
    "VASSAL.configure.StringArrayConfigurer.updateViewable(int)",
    "VASSAL.configure.StringArrayConfigurer.getTextComponent()",
    "VASSAL.configure.StringArrayConfigurer.getTextValue()",
    "VASSAL.configure.StringArrayConfigurer.setTextValue(java.lang.String)",
    "VASSAL.configure.StringArrayConfigurer.addTextActionListener(java.awt.event.ActionListener)",
    "VASSAL.configure.StringArrayConfigurer.updateModel()",
    "VASSAL.build.module.NotesWindow.HOT_KEY",
    "VASSAL.build.module.NotesWindow.ICON",
    "VASSAL.build.module.NotesWindow.TOOLTIP",
    "VASSAL.build.module.NotesWindow.launch",
    "VASSAL.build.module.PieceWindow.mainWindowDock",
    "VASSAL.build.module.DiceButton.launch",
    "VASSAL.build.module.DiceButton.BUTTON_TEXT",
    "VASSAL.build.module.DiceButton.TOOLTIP",
    "VASSAL.build.module.DiceButton.NAME",
    "VASSAL.build.module.DiceButton.ICON",
    "VASSAL.build.module.DiceButton.HOTKEY",
    "VASSAL.build.module.properties.ChangePropertyButton.launch",
    "VASSAL.build.module.GlobalOptions.INITIAL_HEAP",
    "VASSAL.build.module.PlayerRoster.retireButton",
    "VASSAL.build.module.PlayerRoster.getInstance()",
    "VASSAL.build.module.ToolbarMenu.BUTTON_TEXT",
    "VASSAL.build.module.ToolbarMenu.BUTTON_ICON",
    "VASSAL.build.module.ToolbarMenu.BUTTON_HOTKEY",
    "VASSAL.build.module.ToolbarMenu.TOOLTIP",
    "VASSAL.build.module.ToolbarMenu.launch",
    "VASSAL.build.module.Inventory.launch",
    "VASSAL.build.module.Inventory.HOTKEY",
    "VASSAL.build.module.Inventory.BUTTON_TEXT",
    "VASSAL.build.module.Inventory.NAME",
    "VASSAL.build.module.Inventory.ICON",
    "VASSAL.build.module.Inventory.TOOLTIP",
    "VASSAL.build.module.ObscurableOptions.setPrompt(java.lang.String)",
    "VASSAL.build.module.map.MapShader.launch",
    "VASSAL.build.module.map.MassKeyCommand.NAME",
    "VASSAL.build.module.map.MassKeyCommand.ICON",
    "VASSAL.build.module.map.MassKeyCommand.TOOLTIP",
    "VASSAL.build.module.map.MassKeyCommand.launch",
    "VASSAL.build.module.map.LOS_Thread.launch",
    "VASSAL.build.module.map.boardPicker.board.RegionGrid$Config$View.isDragging",
    "VASSAL.build.module.map.LayerControl.NAME",
    "VASSAL.build.module.map.LayerControl.TOOLTIP",
    "VASSAL.build.module.map.LayerControl.BUTTON_TEXT",
    "VASSAL.build.module.map.LayerControl.BUTTON_ICON",
    "VASSAL.build.module.map.LayerControl.BUTTON_HOTKEY",
    "VASSAL.build.module.map.LayerControl.launch",
    "VASSAL.build.module.map.SelectionHighlighter.x",
    "VASSAL.build.module.map.SelectionHighlighter.y",
    "VASSAL.build.module.map.PieceRecenterer.BUTTON_TEXT",
    "VASSAL.build.module.map.PieceRecenterer.ICON",
    "VASSAL.build.module.map.PieceRecenterer.HOTKEY",
    "VASSAL.build.module.map.PieceRecenterer.TOOLTIP",
    "VASSAL.build.module.map.PieceRecenterer.launch",
    "VASSAL.build.module.map.TextSaver.HOTKEY",
    "VASSAL.build.module.map.TextSaver.TOOLTIP",
    "VASSAL.build.module.map.TextSaver.launch",
    "VASSAL.build.module.map.ImageSaver.launch",
    "VASSAL.build.module.DoActionButton.BUTTON_TEXT",
    "VASSAL.build.module.DoActionButton.TOOLTIP",
    "VASSAL.build.module.DoActionButton.NAME",
    "VASSAL.build.module.DoActionButton.HOTKEY",
    "VASSAL.build.module.DoActionButton.ICON",
    "VASSAL.build.module.DoActionButton.launch",
    "VASSAL.build.module.SpecialDiceButton.BUTTON_TEXT",
    "VASSAL.build.module.SpecialDiceButton.TOOLTIP",
    "VASSAL.build.module.SpecialDiceButton.NAME",
    "VASSAL.build.module.SpecialDiceButton.ICON",
    "VASSAL.build.module.SpecialDiceButton.HOTKEY",
    "VASSAL.build.module.documentation.DialogHelpWindow.hyperlinkUpdate(javax.swing.event.HyperlinkEvent)",
    "VASSAL.build.module.documentation.HelpWindow.hyperlinkUpdate(javax.swing.event.HyperlinkEvent)",
    "VASSAL.build.module.documentation.BrowserHelpFile.recursiveDelete(java.io.File)",
    "VASSAL.build.module.Map.mainWindowDock",
    "VASSAL.build.module.Map.launchButton",
    "VASSAL.build.module.Map.appendToTitle(java.lang.String)",
    "VASSAL.build.GameModule.appendToTitle(java.lang.String)",
    "VASSAL.preferences.Prefs.write()",
    "VASSAL.tools.swing.SwingUtils.isLeftMouseButton(java.awt.event.MouseEvent)",
    "VASSAL.tools.swing.SwingUtils.isRightMouseButton(java.awt.event.MouseEvent)",
    "VASSAL.tools.swing.SwingUtils.isControlDown(java.awt.event.MouseEvent)",
    "VASSAL.tools.version.VersionUtils.isUpdateable(java.lang.String)",
    "VASSAL.tools.ErrorDialog.dataError(VASSAL.build.BadDataReport)",
    "VASSAL.tools.NamedKeyStroke.getNamedKeyStroke(char)",
    "VASSAL.tools.NamedKeyStroke.getNamedKeyStroke(char, int)",
    "VASSAL.tools.NamedKeyStroke.getNamedKeyStroke(int, int)",
    "VASSAL.tools.NamedKeyStroke.getKeyStrokeForEvent(java.awt.event.KeyEvent)",
    "VASSAL.counters.Immobilized.Immobilized(VASSAL.counters.GamePiece, java.lang.String)",
    "VASSAL.counters.PieceDefiner.setBaseWindow(VASSAL.build.module.documentation.HelpWindow)",
    "VASSAL.counters.Hideable.setAllHidden(boolean)",
    "VASSAL.counters.BasicPiece.cloneKey",
    "VASSAL.counters.BasicPiece.deleteKey",
    "VASSAL.counters.BasicPiece.image",
    "VASSAL.counters.MultiImagePicker.getImageNames()",
    "VASSAL.counters.Decorator.setOldProperties(VASSAL.counters.GamePiece)",
    "VASSAL.counters.KeyBuffer.getPieces()",
    "VASSAL.counters.SendToLocation$Ed.advancedLabel",
    "VASSAL.counters.SendToLocation$Ed.advancedInput",
    "VASSAL.counters.Translate$Editor.advancedInput",
    "VASSAL.counters.Deck.Deck()",
    "VASSAL.counters.Labeler.CENTER",
    "VASSAL.counters.Labeler.RIGHT",
    "VASSAL.counters.Labeler.LEFT",
    "VASSAL.counters.Labeler.TOP",
    "VASSAL.counters.Labeler.BOTTOM",
    "VASSAL.counters.Labeler.HORIZONTAL_ALIGNMENT",
    "VASSAL.counters.Labeler.VERTICAL_ALIGNMENT",
    "VASSAL.counters.Labeler.imagePainter",
    "VASSAL.counters.Labeler.lastRect",
    "VASSAL.counters.Labeler.lastShape",
    "VASSAL.counters.Labeler.drawLabel(java.awt.Graphics, java.lang.String, int, int, int, int, java.awt.Color, java.awt.Color)",
    "VASSAL.counters.Labeler.drawLabel(java.awt.Graphics, java.lang.String, int, int, java.awt.Font, int, int, java.awt.Color, java.awt.Color, java.awt.Color)",
    "VASSAL.counters.PieceIterator.PieceIterator(java.util.Enumeration, VASSAL.counters.PieceFilter)",
    "VASSAL.counters.PieceIterator.visible(java.util.Enumeration)",
    "VASSAL.counters.Embellishment.lastBounds",
    "VASSAL.counters.Embellishment.lastShape",
    "VASSAL.counters.ActionButton$ButtonPusher.register(java.awt.Component, VASSAL.counters.GamePiece, int, int)",
    "VASSAL.chat.ui.ChatServerControls.splitter",
    "VASSAL.Info.javaBinPath",
    "VASSAL.Info.getScreenBounds(java.awt.Component)",
    "VASSAL.Info.getBinDir()",
    "VASSAL.Info.getHomeDir()",
    // classes
    "VASSAL.launch.BasicModule",
    "VASSAL.build.module.DiceButton$IconConfig",
    "VASSAL.build.module.DoActionButton$IconConfig",
    "VASSAL.build.module.DoActionButton$NamedHotkeyListConfigurer",
    "VASSAL.build.module.Inventory$IconConfig",
    "VASSAL.build.module.NotesWindow$IconConfig",
    "VASSAL.build.module.SpecialDiceButton$IconConfig",
    "VASSAL.build.module.map.ImageSaver$IconConfig",
    "VASSAL.build.module.map.LOS_Thread$IconConfig",
    "VASSAL.build.module.map.MapShader$IconConfig",
    "VASSAL.build.module.map.TextSaver$IconConfig",
    "VASSAL.build.module.map.PieceRecenterer$IconConfig",
    "VASSAL.build.module.map.MassKeyCommand$IconConfig",
    "VASSAL.build.widget.HtmlChart$XTMLEditorKit",
    "VASSAL.tools.BackgroundTask",
    "VASSAL.tools.HTTPPostBuilder",
    "VASSAL.tools.io.IOUtils",
    "VASSAL.tools.image.GeneralFilter$HermiteFilter",
    "VASSAL.tools.image.GeneralFilter$BoxFilter",
    "VASSAL.tools.image.GeneralFilter$TriangleFilter",
    "VASSAL.tools.image.GeneralFilter$BSplineFilter",
    "VASSAL.tools.image.GeneralFilter$BellFilter",
    "VASSAL.tools.FutureUtils",
    "VASSAL.tools.PlayerIdFormattedString",
    "VASSAL.tools.version.VersionTokenizer",
    "VASSAL.tools.version.VassalVersionTokenizer",
    "VASSAL.tools.version.VersionFormatException",
    "VASSAL.tools.ComponentSplitter",
    "VASSAL.tools.Sort",
    "VASSAL.tools.EnumeratedIterator",
    "VASSAL.tools.HashCode",
    "VASSAL.tools.ArrayUtils",
    "VASSAL.tools.ImprovedAveragingScaleFilter",
    "VASSAL.tools.ArrayIterator",
    "VASSAL.tools.logging.LogEntry",
    "VASSAL.tools.logging.LogListener",
    "VASSAL.tools.logging.Logger",
    "VASSAL.counters.PieceImage",
    "VASSAL.counters.DynamicProperty$DynamicKeyCommandConfigurer",
    "VASSAL.counters.ActionButton$ButtonPusher$ComponentMouseListener"
  );

  private final DependencyWalker walker;
  private String thisClass;

  public RemovalAndDeprecationChecker() {
    walker = new DependencyWalker();
  }

  private Pair<Set<String>, Set<String>> walk() {
    final Set<String> removed = new HashSet<>();
    final Set<String> deprecated = new HashSet<>();

    final Consumer<String> callback = s -> {
      if (REMOVED.contains(s)) {
        removed.add(s);
      }
      else if (DEPRECATED.contains(s)) {
        deprecated.add(s);
      }
    };

    walker.setClassCallback(callback);
    walker.setMethodCallback(callback);
    walker.setFieldCallback(callback);

    walker.walk();
    return Pair.of(removed, deprecated); 
  }

  public Pair<Set<String>, Set<String>> check(byte[] classFile) {
    walker.setInput(classFile);
    return walk();
  }

  public Pair<Set<String>, Set<String>> check(InputStream in) throws IOException {
    walker.setInput(in);
    return walk();
  }

  public Pair<Set<String>, Set<String>> check(String className) throws IOException {
    walker.setInput(className);
    return walk();
  }

  public Pair<Map<String, Set<String>>, Map<String, Set<String>>> check(ZipFile zf) throws IOException {
    final Map<String, Set<String>> removed = new HashMap<>();
    final Map<String, Set<String>> deprecated = new HashMap<>();

    walker.setThisClassCallback(s -> thisClass = s);

    final Enumeration<? extends ZipEntry> entries = zf.entries();
    while (entries.hasMoreElements()) {
      final ZipEntry ze = entries.nextElement();
      if (ze.getName().endsWith(".class")) {
        try (InputStream in = zf.getInputStream(ze)) { 
          final Pair<Set<String>, Set<String>> p = check(in);
          // store the results by class
          removed.put(thisClass, p.getLeft());
          deprecated.put(thisClass, p.getRight());
        }
      }
    }
    return Pair.of(removed, deprecated);
  }

  public static String formatResult(Map<String, Set<String>> dmap) {
    final StringBuilder sb = new StringBuilder();
    final List<String> dependers = new ArrayList<>(dmap.keySet());
    Collections.sort(dependers);
    
    for (String dhead: dependers) {
      final List<String> ds = new ArrayList<>(dmap.get(dhead));
      Collections.sort(ds);
      for (String dtail: ds) {
        sb.append(dhead).append(" => ").append(dtail).append('\n');
      }
    }

    return sb.toString();
  }
}
