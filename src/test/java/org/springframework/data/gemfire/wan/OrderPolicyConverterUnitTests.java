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

package org.springframework.data.gemfire.wan;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import com.gemstone.gemfire.cache.util.Gateway;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link OrderPolicyConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.wan.OrderPolicyConverter
 * @see com.gemstone.gemfire.cache.util.Gateway.OrderPolicy
 * @since 1.7.0
 */
@SuppressWarnings("deprecation")
public class OrderPolicyConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private final OrderPolicyConverter converter = new OrderPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void convert() {
		assertThat(converter.convert("key")).isEqualTo(Gateway.OrderPolicy.KEY);
		assertThat(converter.convert("Partition")).isEqualTo(Gateway.OrderPolicy.PARTITION);
		assertThat(converter.convert("THREAD")).isEqualTo(Gateway.OrderPolicy.THREAD);
	}

	@Test
	public void convertIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[process] is not a valid OrderPolicy");

		converter.convert("process");
	}

	@Test
	public void setAsText() {
		assertThat(converter.getValue()).isNull();
		converter.setAsText("PartItIOn");
		assertThat(converter.getValue()).isEqualTo(Gateway.OrderPolicy.PARTITION);
		converter.setAsText("thREAD");
		assertThat(converter.getValue()).isEqualTo(Gateway.OrderPolicy.THREAD);
	}

	@Test
	public void setAsTextWithIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[value] is not a valid OrderPolicy");

		converter.setAsText("value");
	}
}
