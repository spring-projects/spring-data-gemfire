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

import com.gemstone.gemfire.cache.EvictionAction;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link EvictionActionConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.EvictionActionConverter
 * @see com.gemstone.gemfire.cache.EvictionAction
 * @since 1.6.0
 */
public class EvictionActionConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private EvictionActionConverter converter = new EvictionActionConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void convert() {
		assertThat(converter.convert("local_destroy")).isEqualTo(EvictionAction.LOCAL_DESTROY);
		assertThat(converter.convert("None")).isEqualTo(EvictionAction.NONE);
		assertThat(converter.convert("OverFlow_TO_dIsk")).isEqualTo(EvictionAction.OVERFLOW_TO_DISK);
	}

	@Test
	public void convertIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[invalid_value] is not a valid EvictionAction");

		converter.convert("invalid_value");
	}

	@Test
	public void setAsText() {
		assertThat(converter.getValue()).isNull();
		converter.setAsText("Local_Destroy");
		assertThat(converter.getValue()).isEqualTo(EvictionAction.LOCAL_DESTROY);
		converter.setAsText("overflow_to_disk");
		assertThat(converter.getValue()).isEqualTo(EvictionAction.OVERFLOW_TO_DISK);
	}

	@Test
	public void setAsTextWithIllegalValue() {
		try {
			exception.expect(IllegalArgumentException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage("[destroy] is not a valid EvictionAction");

			converter.setAsText("destroy");
		}
		finally {
			assertThat(converter.getValue()).isNull();
		}
	}
}
