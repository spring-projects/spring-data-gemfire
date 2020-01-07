/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
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

import org.apache.geode.cache.util.Gateway;
import org.apache.geode.cache.wan.GatewaySender;

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
 * @see org.apache.geode.cache.util.Gateway.OrderPolicy
 * @since 1.7.0
 */
@SuppressWarnings("deprecation")
public class OrderPolicyConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private final OrderPolicyConverter converter = new OrderPolicyConverter();

	@After
	public void tearDown() {
		this.converter.setValue(null);
	}

	@Test
	public void convert() {

		assertThat(this.converter.convert("key")).isEqualTo(GatewaySender.OrderPolicy.KEY);
		assertThat(this.converter.convert("Partition")).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
		assertThat(this.converter.convert("THREAD")).isEqualTo(GatewaySender.OrderPolicy.THREAD);
	}

	@Test
	public void convertIllegalValue() {

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[process] is not a valid OrderPolicy");

		this.converter.convert("process");
	}

	@Test
	public void setAsText() {

		assertThat(this.converter.getValue()).isNull();

		this.converter.setAsText("PartItIOn");

		assertThat(this.converter.getValue()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);

		this.converter.setAsText("thREAD");

		assertThat(this.converter.getValue()).isEqualTo(GatewaySender.OrderPolicy.THREAD);
	}

	@Test
	public void setAsTextWithIllegalValue() {

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[value] is not a valid OrderPolicy");

		this.converter.setAsText("value");
	}
}
