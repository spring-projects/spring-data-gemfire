/*
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
package org.springframework.data.gemfire.repository.config;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

/**
 * Integration test for {@link GemfireRepositoriesRegistrar} (annotation based
 * repository configuration).
 * 
 * @author Oliver Gierke
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class GemfireRepositoriesRegistrarIntegrationTest {

	@Configuration
	@EnableGemfireRepositories("org.springframework.data.gemfire.repository.sample")
	static class Config {

		@Bean
		public GemFireCache cache() throws Exception {

			CacheFactoryBean factory = new CacheFactoryBean();
			factory.afterPropertiesSet();
			return factory.getObject();
		}

		@Bean
		public Region<Long, Person> simple() throws Exception {

			LocalRegionFactoryBean<Long, Person> factory = new LocalRegionFactoryBean<Long, Person>();
			factory.setCache(cache());
			factory.setName("simple");
			factory.afterPropertiesSet();

			return factory.getObject();
		}
	}

	@Autowired
	PersonRepository repository;

	@Test
	public void bootstrapsRepositoriesCorrectly() {

	}
}