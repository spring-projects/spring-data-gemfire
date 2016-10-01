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

import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link StartupPolicyConverter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.wan.StartupPolicyType
 * @see org.springframework.data.gemfire.wan.StartupPolicyConverter
 * @since 1.6.0
 */
public class StartupPolicyConverterUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private StartupPolicyConverter converter = new StartupPolicyConverter();

	@After
	public void tearDown() {
		converter.setValue(null);
	}

	@Test
	public void convert() {
		assertThat(converter.convert("none")).isEqualTo(StartupPolicyType.NONE);
		assertThat(converter.convert("Primary")).isEqualTo(StartupPolicyType.PRIMARY);
		assertThat(converter.convert("SecONdARY")).isEqualTo(StartupPolicyType.SECONDARY);
	}

	@Test
	public void convertIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[tertiary] is not a valid StartupPolicyType");

		converter.convert("tertiary");
	}

	@Test
	public void setAsText() {
		assertThat(converter.getValue()).isNull();
		converter.setAsText("priMARY");
		assertThat(converter.getValue()).isEqualTo(StartupPolicyType.PRIMARY);
		converter.setAsText("SecondAry");
		assertThat(converter.getValue()).isEqualTo(StartupPolicyType.SECONDARY);
	}

	@Test
	public void setAsTextWithIllegalValue() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("[invalid] is not a valid StartupPolicyType");

		converter.setAsText("invalid");
	}
}
