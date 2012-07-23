/*
 * Copyright 2012 the original author or authors.
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
 */
package org.springframework.data.gemfire.repository.support;

import java.util.Collections;

import org.junit.Test;
import org.springframework.data.gemfire.mapping.Regions;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.test.context.ContextConfiguration;

/**
 * Integration test for {@link GemfireRepositoryFactory}.
 * 
 * @author Oliver Gierke
 */
@ContextConfiguration("../config/repo-context.xml")
public class GemfireRepositoryFactoryIntegrationTests extends AbstractGemfireRepositoryFactoryIntegrationTests {

	@Override
	protected PersonRepository getRepository(Regions regions) {

		GemfireRepositoryFactory factory = new GemfireRepositoryFactory(regions, null);
		return factory.getRepository(PersonRepository.class);
	}

	@Test(expected = IllegalStateException.class)
	@SuppressWarnings("unchecked")
	public void throwsExceptionIfReferencedRegionIsNotConfigured() {

		GemfireRepositoryFactory factory = new GemfireRepositoryFactory((Iterable) Collections.emptySet(), null);
		factory.getRepository(PersonRepository.class);
	}
}
