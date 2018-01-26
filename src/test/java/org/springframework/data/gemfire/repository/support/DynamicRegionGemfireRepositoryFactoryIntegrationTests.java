/*
 * Copyright 2012-2015 the original author or authors.
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

import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Collections;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.LookupRegionFactoryBean;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;
import org.springframework.data.gemfire.mapping.Regions;
import org.springframework.data.gemfire.repository.query.GemfireQueryMethod;
import org.springframework.data.gemfire.repository.query.PartTreeGemfireRepositoryQuery;
import org.springframework.data.gemfire.repository.query.QueryCustomizer;
import org.springframework.data.gemfire.repository.query.QueryString;
import org.springframework.data.gemfire.repository.query.StringBasedGemfireRepositoryQuery;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.data.projection.ProjectionFactory;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.NamedQueries;
import org.springframework.data.repository.core.RepositoryMetadata;
import org.springframework.data.repository.query.EvaluationContextProvider;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.RepositoryQuery;
import org.springframework.test.context.ContextConfiguration;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.Region;

/**
 * Integration test for Custom Gemfire repository with a Query Customizer implementation.
 *
 * @see SGF-713
 * @author Jebuselwyn Martin
 */
@ContextConfiguration("../config/repo-context.xml")
public class DynamicRegionGemfireRepositoryFactoryIntegrationTests
		extends AbstractGemfireRepositoryFactoryIntegrationTests {

	@Autowired ApplicationContext context;
	@Autowired GemfireMappingContext mappingContext;

	private static final ThreadLocal<Integer> regionPrefix = new ThreadLocal<Integer>();

	/**
	 * Setting the thread-local prefix - in real scenarios this will be set by the calling client
	 */
	@BeforeClass
	public static void setup() {
		regionPrefix.set(1);
	}

	/**
	 * Setting up the data for the tests with the Dynamic gemfire template
	 */
	@Before
	public void setUp() {
		dave = new Person(1L, "Dave", "Matthews");
		carter = new Person(2L, "Carter", "Beauford");
		boyd = new Person(3L, "Boyd", "Tinsley");
		stefan = new Person(4L, "Stefan", "Lessard");
		leroi = new Person(5L, "Leroi", "Moore");
		jeff = new Person(6L, "Jeff", "Coffin");
		oliverAugust = new Person(7L, "Oliver August", "Matthews");

		GemfireMappingContext context = new GemfireMappingContext();

		Regions regions = new Regions(this.regions, context);
		GemfireTemplate template = new DynamicRegionGemfireTemplate(new GemfireTemplate(regions.getRegion(Person.class)));

		template.put(dave.id, dave);
		template.put(carter.id, carter);
		template.put(boyd.id, boyd);
		template.put(stefan.id, stefan);
		template.put(leroi.id, leroi);
		template.put(jeff.id, jeff);
		template.put(oliverAugust.id, oliverAugust);

		repository = getRepository(regions);
	}

	@Override
	protected PersonRepository getRepository(Regions regions) {

		GemfireRepositoryFactory factory = new DynamicRegionNameGemfireFactory(regions, mappingContext);
		return factory.getRepository(PersonRepository.class);
	}

	@Test(expected = IllegalStateException.class)
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void throwsExceptionIfReferencedRegionIsNotConfigured() {

		GemfireRepositoryFactory factory = new DynamicRegionNameGemfireFactory((Iterable) Collections.emptySet(),
				mappingContext);
		factory.getRepository(PersonRepository.class);
	}

	/*
		Query Customizer - sample implementation
	 */
	class DynamicRegionNameQueryCustomizer implements QueryCustomizer {

		Logger logger = LoggerFactory.getLogger(DynamicRegionNameQueryCustomizer.class);

		@Override
		public QueryString customizeQuery(QueryString query) {
			String queryString = query.toString();
			String currRegionName = "simple";
			String newRegionName = getDynamicRegionName(currRegionName);
			queryString = queryString.replaceAll(currRegionName, newRegionName);
			logger.warn("Query generated {}", queryString);
			System.out.println("Query generated " + queryString);
			return new QueryString(queryString);
		}
	}

	/**
	 * Dynamic region name from thread-local id
	 *
	 * @param currRegionName
	 * @return
	 */
	private String getDynamicRegionName(String currRegionName) {
		return MessageFormat.format("user{0}_{1}", regionPrefix.get(), currRegionName);
	}

	/**
	 * Custom Gemfire factory to invoke the StringRepsotiory with queryCustomizer constructor
	 */
	class DynamicRegionNameGemfireFactory extends GemfireRepositoryFactory {

		Logger logger = LoggerFactory.getLogger(DynamicRegionNameGemfireFactory.class);

		private final MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext;

		private final Regions regions;

		/**
		 * Creates a new {@link GemfireRepositoryFactory}.
		 *
		 * @param regions must not be {@literal null}.
		 * @param mappingContext the {@link MappingContext} used by the constructed Repository for mapping entities to the
		 *          underlying data store, must not be {@literal null}.
		 */
		public DynamicRegionNameGemfireFactory(Iterable<Region<?, ?>> regions,
				MappingContext<? extends GemfirePersistentEntity<?>, GemfirePersistentProperty> mappingContext) {

			super(regions, mappingContext);
			this.mappingContext = mappingContext;
			this.regions = new Regions(regions, this.mappingContext);
		}

		/*
		     * (non-Javadoc)
		     *
		     * @see org.springframework.data.repository.core.support.RepositoryFactorySupport
		     * 	#getQueryLookupStrategy(Key, EvaluationContextProvider)
		     */
		@Override
		protected QueryLookupStrategy getQueryLookupStrategy(QueryLookupStrategy.Key key,
				EvaluationContextProvider evaluationContextProvider) {
			logger.info("SimulationGemfireFactory getQueryLookupStrategy {}", key);

			return new QueryLookupStrategy() {

				@Override
				public RepositoryQuery resolveQuery(Method method, RepositoryMetadata metadata, ProjectionFactory factory,
						NamedQueries namedQueries) {

					GemfireQueryMethod queryMethod = new GemfireQueryMethod(method, metadata, factory, mappingContext);
					GemfireTemplate template = getTemplate(metadata);

					QueryCustomizer queryCustomizer = new DynamicRegionNameQueryCustomizer();
					if (queryMethod.hasAnnotatedQuery()) {
						return new StringBasedGemfireRepositoryQuery(queryMethod, template, queryCustomizer).asUserDefinedQuery();
					}

					if (namedQueries.hasQuery(queryMethod.getNamedQueryName())) {
						return new StringBasedGemfireRepositoryQuery(namedQueries.getQuery(queryMethod.getNamedQueryName()),
								queryMethod, template, queryCustomizer).asUserDefinedQuery();
					}

					return new PartTreeGemfireRepositoryQuery(queryMethod, template, queryCustomizer);
				}
			};
		}

	}

	/**
	 * Custom Gemfire template which overrides the region-name
	 * 
	 * @param <T>
	 * @param <ID>
	 */
	class DynamicRegionGemfireTemplate<T, ID> extends GemfireTemplate {

		private EntityInformation entityInformation = null;

		private Region<?, T> primaryRegion;

		public DynamicRegionGemfireTemplate(GemfireTemplate gemfireTemplate, EntityInformation entityInformation) {
			this.primaryRegion = gemfireTemplate.getRegion();
			this.entityInformation = entityInformation;
		}

		public DynamicRegionGemfireTemplate(GemfireTemplate gemfireTemplate) {
			this.primaryRegion = gemfireTemplate.getRegion();
		}

		@Override
		public Region<?, ?> getRegion() {
			System.out.println("Get Region Called");
			try {
				return getSnapshotRegion(primaryRegion.getName()).getObject();
			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}
		}

		public LookupRegionFactoryBean<?, ?> getSnapshotRegion(String regionName) throws Exception {
			System.out.println("LookupRegionFactoryBean Called");
			LookupRegionFactoryBean<?, ?> region = new LookupRegionFactoryBean<Person, Long>();
			region.setCache(CacheFactory.getAnyInstance());
			region.setName(getDynamicRegionName(regionName));
			region.setLookupEnabled(true);
			region.afterPropertiesSet();
			return region;
		}
	}
}
