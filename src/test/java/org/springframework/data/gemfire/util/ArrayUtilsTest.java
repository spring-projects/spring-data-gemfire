/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.util;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

/**
 * The ArrayUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the ArrayUtils class.
 *
 * @author John Blum
 * @see java.util.Arrays
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.util.ArrayUtils
 * @since 1.7.0
 */
public class ArrayUtilsTest {

	@Test
	public void insertAtBeginning() {
		Object[] originalArray = { "testing", "tested" };
		Object[] newArray = ArrayUtils.insert(originalArray, 0, "test");

		assertNotSame(originalArray, newArray);
		assertFalse(Arrays.equals(originalArray, newArray));
		assertEquals("test", newArray[0]);
		assertEquals("testing", newArray[1]);
		assertEquals("tested", newArray[2]);
	}

	@Test
	public void insertInMiddle() {
		Object[] originalArray = { "test", "tested" };
		Object[] newArray = ArrayUtils.insert(originalArray, 1, "testing");

		assertNotSame(originalArray, newArray);
		assertFalse(Arrays.equals(originalArray, newArray));
		assertEquals("test", newArray[0]);
		assertEquals("testing", newArray[1]);
		assertEquals("tested", newArray[2]);
	}

	@Test
	public void insertAtEnd() {
		Object[] originalArray = { "test", "testing" };
		Object[] newArray = ArrayUtils.insert(originalArray, 2, "tested");

		assertNotSame(originalArray, newArray);
		assertFalse(Arrays.equals(originalArray, newArray));
		assertEquals("test", newArray[0]);
		assertEquals("testing", newArray[1]);
		assertEquals("tested", newArray[2]);
	}

	@Test
	public void isEmpty() {
		assertFalse(ArrayUtils.isEmpty("test", "testing", "tested"));
		assertFalse(ArrayUtils.isEmpty("test"));
		assertFalse(ArrayUtils.isEmpty(""));
		assertFalse(ArrayUtils.isEmpty(null, null, null));
		assertTrue(ArrayUtils.isEmpty());
		assertTrue(ArrayUtils.isEmpty((Object[]) null));
	}

	@Test
	public void length() {
		assertEquals(3, ArrayUtils.length("test", "testing", "tested"));
		assertEquals(1, ArrayUtils.length("test"));
		assertEquals(1, ArrayUtils.length(""));
		assertEquals(3, ArrayUtils.length(null, null, null));
		assertEquals(0, ArrayUtils.length());
		assertEquals(0, ArrayUtils.length((Object[]) null));
	}

	@Test
	public void nullSafeArrayWithNonNullArray() {
		String[] stringArray = { "test", "testing", "tested" };

		assertThat(ArrayUtils.nullSafeArray(stringArray), is(sameInstance(stringArray)));

		Double[] emptyDoubleArray = {};

		assertThat(ArrayUtils.nullSafeArray(emptyDoubleArray), is(sameInstance(emptyDoubleArray)));

		Integer[] numberArray = { 1, 2, 3 };

		assertThat(ArrayUtils.nullSafeArray(numberArray), is(sameInstance(numberArray)));

		Character[] characterArray = { 'A', 'B', 'C' };

		assertThat(ArrayUtils.nullSafeArray(characterArray), is(sameInstance(characterArray)));
	}

	@Test
	public void nullSafeArrayWithNullArray() {
		Object array = ArrayUtils.nullSafeArray(null);

		assertThat(array, is(instanceOf(Object[].class)));
		assertThat(((Object[]) array).length, is(equalTo(0)));
	}

	@Test
	public void removeFromBeginning() {
		Object[] originalArray = { "test", "testing", "tested" };
		Object[] newArray = ArrayUtils.remove(originalArray, 0);

		assertNotSame(originalArray, newArray);
		assertFalse(Arrays.equals(originalArray, newArray));
		assertEquals("testing", newArray[0]);
		assertEquals("tested", newArray[1]);
	}

	@Test
	public void removeFromMiddle() {
		Object[] originalArray = { "test", "testing", "tested" };
		Object[] newArray = ArrayUtils.remove(originalArray, 1);

		assertNotSame(originalArray, newArray);
		assertFalse(Arrays.equals(originalArray, newArray));
		assertEquals("test", newArray[0]);
		assertEquals("tested", newArray[1]);
	}

	@Test
	public void removeFromEnd() {
		Object[] originalArray = { "test", "testing", "tested" };
		Object[] newArray = ArrayUtils.remove(originalArray, 2);

		assertNotSame(originalArray, newArray);
		assertFalse(Arrays.equals(originalArray, newArray));
		assertEquals("test", newArray[0]);
		assertEquals("testing", newArray[1]);
	}

}
