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

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.PagingAndSortingRepository;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;

/**
 * Unit tests for {@link GemfireRepositoryFactory}.
 * 
 * @author Oliver Gierke
 */
@RunWith(MockitoJUnitRunner.class)
public class GemfireRepositoryFactoryUnitTests {

	@Mock
	Region<?, ?> region;

	@Mock
	@SuppressWarnings("rawtypes")
	RegionAttributes attributes;

	/**
	 * @see SGF-112
	 */
	@Test
	@SuppressWarnings("unchecked")
	public void rejectsInterfacesExtendingPagingAndSortingRepository() {

		when(region.getName()).thenReturn("simple");
		when(region.getAttributes()).thenReturn(attributes);

		List<Region<?, ?>> regions = new ArrayList<Region<?, ?>>();
		regions.add(region);

		GemfireMappingContext context = new GemfireMappingContext();

		GemfireRepositoryFactory factory = new GemfireRepositoryFactory(regions, context);

		try {
			factory.getRepository(SampleInterface.class);
		} catch (IllegalStateException e) {
			assertThat(e.getMessage(), Matchers.startsWith("Pagination is not supported by Gemfire repositories!"));
		}
	}

	interface SampleInterface extends PagingAndSortingRepository<Person, Long> {

	}
}
