/*
 * Copyright 2020 Vassal Development Team
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
package VASSAL.counters;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.tools.DataArchive;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import java.awt.image.BufferedImage;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import javax.swing.ImageIcon;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

/**
 * Base class for all Decorator Tests. Provides
 */
public class DecoratorTest {

  /**
   * Run the serialization tests on the supplied Decorator.
   *
   *
   * @param test Test Description
   * @param referenceTrait The reference Trait to be tested. The Reference trait does not need to have a BasicPiece as an Inner piece,
   *                       but one will be added if it does not.
   */
  public void serializeTest(String test, Decorator referenceTrait) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException, InstantiationException {

    // Create a dummy image to return from mocks
    BufferedImage dummyImage = new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR);

    // Create a dummy ImageOp before mocking Op
    ImageOp dummyOp = Op.load(dummyImage);

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      // Create a static mock for IconFactory and return the Calculator icon when asked. Allows Editors with Beanshell configurers to initialise.
      try (MockedStatic<IconFactory> staticIf = Mockito.mockStatic(IconFactory.class)) {
        // Mock Op.load to return a dummy image when requested
        try (MockedStatic<Op> staticOp = Mockito.mockStatic(Op.class)) {

          // Now return the dummy image when asked instead of looking in the archive
          staticOp.when(() -> Op.load(any(String.class))).thenReturn(dummyOp);

          // Return Dummy icons from IconFactory
          staticIf.when(() -> IconFactory.getIcon(any(String.class), anyInt())).thenReturn(new ImageIcon(dummyImage));

          // Mock DataArchive to return a list of image names
          final DataArchive da = mock(DataArchive.class);
          when(da.getImageNames()).thenReturn(new String[0]);

          // Mock some GpID Support
          final GpIdSupport gpid = mock(GpIdSupport.class);

          // Mock GameModule to return various resources
          final GameModule gm = mock(GameModule.class);
          when(gm.getDataArchive()).thenReturn(da);
          when(gm.getGpIdSupport()).thenReturn(gpid);

          staticGm.when(GameModule::getGameModule).thenReturn(gm);

          // Can't test without a properly constructed BasicPiece as the inner
          if (referenceTrait.getInner() == null) {
            referenceTrait.setInner(createBasicPiece());
          }

          // Test we can reconstruct a Piece by passing its type through its Constructor
          constructorTest(test, referenceTrait);

          // Test we can reconstruct a Piece using the BasicCommandEncoder
          commandEncoderTest(test, referenceTrait);

          // Test the serialization used in the internal editor matches the standard serialization
          editorTest(test, referenceTrait);
        }
      }
    }
  }

  /**
   * Create and return a standard BasiCPiece trait with enough internals set up to
   * be used for simple tests
   *
   * @return Generated BasicPiece
   */
  public BasicPiece createBasicPiece() {
    final BasicPiece bp = new BasicPiece();
    bp.setProperty(Properties.PIECE_ID, "1");
    return bp;
  }

  /**
   * Test that a trait's internal editor encodes the type in the same way
   * that the trait started with. Checks for serialization issues in the trait editors.
   *
   * NOTE: Don't test State across the Editor, Trait Editors don't need to maintain state
   *
   * @param test A descriptive name for this test or test sequence
   * @param referenceTrait The trait to be tested
   */
  public void editorTest(String test, Decorator referenceTrait) {

    // Save the original Type in case the Editor changes them
    final String originalType = referenceTrait.myGetType();

    // Create an instance of the Editor
    final PieceEditor editor = referenceTrait.getEditor();

    // Confirm that the Type  encoded by the editor is the same as the original trait
    assertThat("Trait Edit Test (Type): " + test, editor.getType(), is(equalTo(originalType))); // NON-NLS
  }

  /**
   * Test that the BasicCommandEncoder can faithfully reproduce this trait
   *
   * @param test Test description
   * @param referenceTrait Reference Trait to be tested
   */
  public void commandEncoderTest(String test, Decorator referenceTrait) {
    final BasicCommandEncoder ce = new BasicCommandEncoder();

    // Create a new trait using the standard Command Encoder and the type from the reference trait
    Decorator testTrait = ce.createDecorator(referenceTrait.myGetType(), new BasicPiece());

    // Inject any State
    testTrait.mySetState(referenceTrait.myGetState());

    // Test constructed trait is equivalent to the reference trait
    assertThat("Command Encoder Test: " + test, referenceTrait.testEquals(testTrait), is(true)); // NON-NLS
  }

  /**
   * Test that a constructed Decorator with the type injected via its 2-arg constructor equals the reference trait
   *
   * @param test Test Description
   * @param referenceTrait Reference trait for comparison
   */
  public void constructorTest(String test, Decorator referenceTrait) throws IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {
    // Build a new trait using the Type from the reference trait
    Constructor<? extends Decorator>  constructor = referenceTrait.getClass().getConstructor(String.class, GamePiece.class);
    Decorator constructedTrait = constructor.newInstance(referenceTrait.myGetType(), createBasicPiece());

    // Inject the state
    constructedTrait.mySetState(referenceTrait.myGetState());

    // Test constructed trait is equivalent to the reference trait
    assertThat("Constructor Test: " + test, referenceTrait.testEquals(constructedTrait), is(true)); // NON-NLS
  }
}
