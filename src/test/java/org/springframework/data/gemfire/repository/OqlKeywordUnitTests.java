/*
 * Copyright 2012-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.repository;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.data.gemfire.repository.query.support.OqlKeyword;

/**
 * Test suite of test cases testing the contract and functionality
 * of the {@link OqlKeyword} enumerated type.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.query.support.OqlKeyword
 * @since 1.0.0
 */
public class OqlKeywordUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void valueOfIgnoreCaseWithEnumeratedValuesIsSuccessful() {
		for (OqlKeyword oqlKeyword : OqlKeyword.values()) {
			assertThat(OqlKeyword.valueOfIgnoreCase(oqlKeyword.getKeyword())).isEqualTo(oqlKeyword);
		}
	}

	@Test
	public void valueOfIgnoreCaseWithUnconventionalValuesIsSuccessful() {
		assertThat(OqlKeyword.valueOfIgnoreCase("and")).isEqualTo(OqlKeyword.AND);
		assertThat(OqlKeyword.valueOfIgnoreCase("As")).isEqualTo(OqlKeyword.AS);
		assertThat(OqlKeyword.valueOfIgnoreCase("CoUnT")).isEqualTo(OqlKeyword.COUNT);
		assertThat(OqlKeyword.valueOfIgnoreCase(" DISTINCT  ")).isEqualTo(OqlKeyword.DISTINCT);
		assertThat(OqlKeyword.valueOfIgnoreCase("  Order BY ")).isEqualTo(OqlKeyword.ORDER_BY);
	}

	@Test
	public void valueOfIgnoreCaseWithIllegalValuesThrowsIllegalArgumentException() {
		assertIllegalOqlKeyword("AN");
		assertIllegalOqlKeyword("ASS");
		assertIllegalOqlKeyword("CNT");
		assertIllegalOqlKeyword("EXTINCT");
		assertIllegalOqlKeyword("TO");
		assertIllegalOqlKeyword("EXPORT");
		assertIllegalOqlKeyword("OUT");
		assertIllegalOqlKeyword("IS DEFINED");
		assertIllegalOqlKeyword("IS-UNDEFINED");
		assertIllegalOqlKeyword("UNLIKE");
		assertIllegalOqlKeyword("NIL");
		assertIllegalOqlKeyword("NULL VALUE");
		assertIllegalOqlKeyword("XOR");
		assertIllegalOqlKeyword("ORDER_BY");
		assertIllegalOqlKeyword("INSERT");
		assertIllegalOqlKeyword("UPDATE");
		assertIllegalOqlKeyword("LIST");
		assertIllegalOqlKeyword("CLASS");
		assertIllegalOqlKeyword("WHAT");
		assertIllegalOqlKeyword("WHEN");
	}

	protected void assertIllegalOqlKeyword(String keyword) {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(String.format("[%s] is not a valid GemFire OQL Keyword", keyword));

		OqlKeyword.valueOfIgnoreCase(keyword);
	}

	@Test
	public void getKeywordEqualsNameExceptForOrderBy() {
		for (OqlKeyword oqlKeyword : OqlKeyword.values()) {
			if (!OqlKeyword.ORDER_BY.equals(oqlKeyword)) {
				assertThat(oqlKeyword.getKeyword()).isEqualTo(oqlKeyword.name());
			}
		}
	}
}
