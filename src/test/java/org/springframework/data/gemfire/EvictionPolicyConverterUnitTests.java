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

package org.springframework.data.gemfire;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link EvictionPolicyConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.EvictionPolicyConverter
 * @see org.springframework.data.gemfire.EvictionPolicyType
 * @since 1.6.0
 */
public class EvictionPolicyConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private final EvictionPolicyConverter converter = new EvictionPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void convert() {
		assertThat(converter.convert("entry_count")).isEqualTo(EvictionPolicyType.ENTRY_COUNT);
		assertThat(converter.convert("Heap_Percentage")).isEqualTo(EvictionPolicyType.HEAP_PERCENTAGE);
		assertThat(converter.convert("MEMorY_SiZe")).isEqualTo(EvictionPolicyType.MEMORY_SIZE);
		assertThat(converter.convert("NONE")).isEqualTo(EvictionPolicyType.NONE);
	}

	@Test
	public void convertIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[LIFO_MEMORY] is not a valid EvictionPolicyType");

		converter.convert("LIFO_MEMORY");
	}

	@Test
	public void setAsText() {
		assertThat(converter.getValue()).isNull();
		converter.setAsText("heap_percentage");
		assertThat(converter.getValue()).isEqualTo(EvictionPolicyType.HEAP_PERCENTAGE);
		converter.setAsText("NOne");
		assertThat(converter.getValue()).isEqualTo(EvictionPolicyType.NONE);
	}

	@Test
	public void setAsTextWithIllegalValue() {
		try {
			exception.expect(IllegalArgumentException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage("[LRU_COUNT] is not a valid EvictionPolicyType");

			converter.setAsText("LRU_COUNT");
		}
		finally {
			assertThat(converter.getValue()).isNull();
		}
	}
}
