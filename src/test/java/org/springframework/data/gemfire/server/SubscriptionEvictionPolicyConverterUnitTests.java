/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.server;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link SubscriptionEvictionPolicyConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicyConverter
 * @since 1.6.0
 */
public class SubscriptionEvictionPolicyConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private final SubscriptionEvictionPolicyConverter converter = new SubscriptionEvictionPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void convert() {
		assertThat(converter.convert("EnTry")).isEqualTo(SubscriptionEvictionPolicy.ENTRY);
		assertThat(converter.convert("MEM")).isEqualTo(SubscriptionEvictionPolicy.MEM);
		assertThat(converter.convert("nONE")).isEqualTo(SubscriptionEvictionPolicy.NONE);
		assertThat(converter.convert("NOne")).isEqualTo(SubscriptionEvictionPolicy.NONE);
	}

	@Test
	public void convertIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[memory] is not a valid SubscriptionEvictionPolicy");

		converter.setAsText("memory");
	}

	@Test
	public void setAsText() {
		assertThat(converter.getValue()).isNull();
		converter.setAsText("enTRY");
		assertThat(converter.getValue()).isEqualTo(SubscriptionEvictionPolicy.ENTRY);
		converter.setAsText("MEm");
		assertThat(converter.getValue()).isEqualTo(SubscriptionEvictionPolicy.MEM);
	}

	@Test
	public void setAsTextWithIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[KEYS] is not a valid SubscriptionEvictionPolicy");

		converter.setAsText("KEYS");
	}
}
