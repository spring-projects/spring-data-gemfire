/*
 * Copyright 2012-2020 the original author or authors.
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
 */

package org.springframework.data.gemfire.repository.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.util.Collections;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.sample.PersonRepository;

/**
 * The GemfireRepositoryFactoryBeanTest class is test suite of test cases testing the contract and functionality
 * of the GemfireRepositoryFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean
 * @since 1.6.3
 */
@SuppressWarnings("rawtypes")
public class GemfireRepositoryFactoryBeanTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	GemfireRepositoryFactoryBean repositoryFactoryBean;

	@Before
	public void setup() {
		repositoryFactoryBean = new GemfireRepositoryFactoryBean(PersonRepository.class);
	}

	@Test
	public void rejectsMappingContextNotSet() {

		exception.expect(IllegalStateException.class);
		exception.expectMessage("GemfireMappingContext");

		repositoryFactoryBean.afterPropertiesSet();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void initializesWithMappingContext() {

		RegionAttributes<?, ?> attributes = mock(RegionAttributes.class);
		doReturn(Long.class).when(attributes).getKeyConstraint();

		Region<?, ?> region = mock(Region.class);
		doReturn("simple").when(region).getName();
		doReturn(attributes).when(region).getAttributes();

		ApplicationContext applicationContext = mock(ApplicationContext.class);
		doReturn(Collections.singletonMap("simple", region)).when(applicationContext).getBeansOfType(Region.class);

		repositoryFactoryBean.setApplicationContext(applicationContext);
		repositoryFactoryBean.setGemfireMappingContext(new GemfireMappingContext());
		repositoryFactoryBean.afterPropertiesSet();

		assertThat(repositoryFactoryBean.getObject(), is(notNullValue()));
	}
}
