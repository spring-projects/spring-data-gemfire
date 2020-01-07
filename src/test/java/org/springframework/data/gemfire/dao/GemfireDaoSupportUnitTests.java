/*
 * Copyright 2016-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.springframework.data.gemfire.dao;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import org.apache.geode.cache.Region;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.data.gemfire.GemfireOperations;
import org.springframework.data.gemfire.GemfireTemplate;

/**
 * Unit tests for {@link GemfireDaoSupport}.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireDaoSupportUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	public Region<?, ?> mockRegion;

	@Test
	public void setAndGetGemfireTemplate() {

		GemfireTemplate expectedGemfireTemplate = new GemfireTemplate(mockRegion);
		GemfireDaoSupport dao = new TestGemfireDaoSupport();

		assertThat(dao.getGemfireTemplate()).isNull();

		dao.setGemfireTemplate(expectedGemfireTemplate);

		assertThat(dao.getGemfireTemplate()).isSameAs(expectedGemfireTemplate);

		dao.setGemfireTemplate(null);

		assertThat(dao.getGemfireTemplate()).isNull();
	}

	@Test
	public void setRegion() {

		GemfireDaoSupport dao = new TestGemfireDaoSupport();

		dao.setRegion(mockRegion);

		GemfireOperations gemfireTemplate = dao.getGemfireTemplate();

		assertThat(gemfireTemplate).isNotNull();
		assertThat(gemfireTemplate).isInstanceOf(GemfireTemplate.class);
		assertThat(((GemfireTemplate) gemfireTemplate).getRegion()).isSameAs(mockRegion);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void createProperlyInitializedGemfireDaoSupportWithTemplate() throws Exception {

		GemfireTemplate expectedGemfireTemplate = new GemfireTemplate();
		GemfireDaoSupport dao = new TestGemfireDaoSupport();

		dao.setGemfireTemplate(expectedGemfireTemplate);
		dao.afterPropertiesSet();

		assertThat(dao.getGemfireTemplate()).isNotNull();
		assertThat(dao.getGemfireTemplate()).isEqualTo(expectedGemfireTemplate);
	}

	@Test
	public void invalidGemfireDaoSupportInstanceThrowsIllegalStateException() throws Exception {

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("A Cache Region or instance of GemfireTemplate is required");

		new TestGemfireDaoSupport().afterPropertiesSet();
	}

	private static final class TestGemfireDaoSupport extends GemfireDaoSupport {
	}
}
