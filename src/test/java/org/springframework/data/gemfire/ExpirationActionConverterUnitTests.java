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

import com.gemstone.gemfire.cache.ExpirationAction;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link ExpirationActionConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.ExpirationActionConverter
 * @since 1.6.0
 */
public class ExpirationActionConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private final ExpirationActionConverter converter = new ExpirationActionConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void convert() {
		assertThat(converter.convert("destroy")).isEqualTo(ExpirationAction.DESTROY);
		assertThat(converter.convert("inValidAte")).isEqualTo(ExpirationAction.INVALIDATE);
		assertThat(converter.convert("LOCAL_dEsTrOy")).isEqualTo(ExpirationAction.LOCAL_DESTROY);
		assertThat(converter.convert("Local_Invalidate")).isEqualTo(ExpirationAction.LOCAL_INVALIDATE);
	}

	@Test
	public void convertIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[illegal_value] is not a valid ExpirationAction");

		converter.convert("illegal_value");
	}

	@Test
	public void setAsText() {
		assertThat(converter.getValue()).isNull();
		converter.setAsText("InValidAte");
		assertThat(converter.getValue()).isEqualTo(ExpirationAction.INVALIDATE);
		converter.setAsText("Local_Destroy");
		assertThat(converter.getValue()).isEqualTo(ExpirationAction.LOCAL_DESTROY);
	}

	@Test
	public void setAsTextWithIllegalValue() {
		try {
			exception.expect(IllegalArgumentException.class);
			exception.expectCause(is(nullValue(Throwable.class)));
			exception.expectMessage("[destruction] is not a valid ExpirationAction");

			converter.setAsText("destruction");
		}
		finally {
			assertThat(converter.getValue()).isNull();
		}
	}
}
