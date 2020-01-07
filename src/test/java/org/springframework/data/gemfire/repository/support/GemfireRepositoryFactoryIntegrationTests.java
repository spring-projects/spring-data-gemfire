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

import java.util.Collections;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.Regions;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.repository.support.Repositories;
import org.springframework.test.context.ContextConfiguration;

/**
 * Integration test for {@link GemfireRepositoryFactory}.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
@ContextConfiguration("../config/repo-context.xml")
public class GemfireRepositoryFactoryIntegrationTests extends AbstractGemfireRepositoryFactoryIntegrationTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Autowired
	private GemfireMappingContext mappingContext;

	@Override
	protected PersonRepository getRepository(Regions regions) {
		return new GemfireRepositoryFactory(regions, mappingContext).getRepository(PersonRepository.class);
	}

	@Test(expected = IllegalStateException.class)
	public void throwsExceptionIfReferencedRegionIsNotConfigured() {
		new GemfireRepositoryFactory(Collections.emptySet(), mappingContext).getRepository(PersonRepository.class);
	}

	/**
	 * @see SGF-140
	 */
	@Test
	public void exposesPersistentProperty() {
		Repositories repositories = new Repositories(applicationContext);
		PersistentEntity<?, ?> entity = repositories.getPersistentEntity(Person.class);

		assertThat(entity, is(notNullValue()));
	}
}
