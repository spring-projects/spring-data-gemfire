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
 *
 */

package org.springframework.data.gemfire.repository.cdi;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.inject.Produces;

import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.RegionShortcut;

/**
 * The GemfireCacheRegionProducer class is an application scoped CDI context bean that is responsible
 * for creating the GemFire Cache "People" Region used to store {@link Person} instances.
 *
 * @author John Blum
 * @see javax.enterprise.context.ApplicationScoped
 * @see javax.enterprise.inject.Produces
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.CacheFactory
 * @see com.gemstone.gemfire.cache.Region
 * @see com.gemstone.gemfire.cache.RegionFactory
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public class GemfireCacheRegionProducer {

	@Produces
	@ApplicationScoped
	public Region<Long, Person> createPeopleRegion() {
		Cache gemfireCache = new CacheFactory()
			.set("name", "SpringDataGemFireCdiTest")
			.set("mcast-port", "0")
			.set("log-level", "warning")
			.create();

		RegionFactory<Long, Person> peopleRegionFactory = gemfireCache.createRegionFactory(RegionShortcut.REPLICATE);

		peopleRegionFactory.setKeyConstraint(Long.class);
		peopleRegionFactory.setValueConstraint(Person.class);

		Region<Long, Person> peopleRegion = peopleRegionFactory.create("People");

		Assert.notNull(peopleRegion);

		return peopleRegion;
	}

}
