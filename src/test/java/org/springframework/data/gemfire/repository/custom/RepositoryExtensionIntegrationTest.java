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
package org.springframework.data.gemfire.repository.custom;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Set;

import org.junit.Test;
import org.springframework.data.annotation.Id;
import org.springframework.data.domain.Pageable;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactory;
import org.springframework.data.repository.Repository;

import com.gemstone.gemfire.cache.Region;

/**
 * Integration tests for repository customizing.
 * 
 * @author Oliver Gierke
 */
public class RepositoryExtensionIntegrationTest {

	/**
	 * @see SGF-130
	 */
	@Test
	public void bootstrapsRepositoryWithCustomImplementationUsingAPageable() throws Exception {

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();
		cacheFactoryBean.afterPropertiesSet();

		RegionFactoryBean<String, Object> regionFactoryBean = new RegionFactoryBean<String, Object>();
		regionFactoryBean.setCache(cacheFactoryBean.getObject());
		regionFactoryBean.setName("SampleEntity");
		regionFactoryBean.afterPropertiesSet();

		Region<String, Object> region = regionFactoryBean.getObject();

		GemfireMappingContext context = new GemfireMappingContext();
		Set<Region<?, ?>> regions = new HashSet<Region<?, ?>>();
		regions.add(region);

		GemfireRepositoryFactory factory = new GemfireRepositoryFactory(regions, context);
		SampleRepository repository = factory.getRepository(SampleRepository.class);

		assertThat(repository, is(notNullValue()));
	}

	interface SampleRepository extends Repository<SampleEntity, Long> {

	}

	interface SampleRepositoryCustom {

		void myCustomMethod(Pageable pageable);
	}

	class SampleRepositoryImpl implements SampleRepositoryCustom {

		public void myCustomMethod(Pageable pageable) {

		}
	}

	class SampleEntity {

		@Id
		Long id;
	}
}
