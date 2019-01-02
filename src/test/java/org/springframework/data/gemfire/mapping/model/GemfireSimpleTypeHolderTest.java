/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.mapping.model;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;
import org.springframework.data.mapping.model.SimpleTypeHolder;

/**
 * The GemfireSimpleTypeHolderTest class is a test suite of test cases testing the contract and functionality
 * of the GemfireSimpleTypeHolder class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.mapping.model.GemfireSimpleTypeHolder
 * @see org.springframework.data.mapping.model.SimpleTypeHolder
 * @since 1.6.3
 */
public class GemfireSimpleTypeHolderTest {

	private final GemfireSimpleTypeHolder holder = new GemfireSimpleTypeHolder(new SimpleTypeHolder());

	@Test
	public void bigDecimalAndBigIntegerAreSimpleTypes() {
		assertThat(holder.isSimpleType(BigDecimal.class), is(true));
		assertThat(holder.isSimpleType(BigInteger.class), is(true));
	}

	@Test
	public void personIsNotASimpleType() {
		assertThat(holder.isSimpleType(Person.class), is(false));
	}

	class Person {
	}

}
