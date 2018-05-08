/*
 * Copyright 2002-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionFactory;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.management.internal.cli.domain.RegionInformation;
import org.apache.geode.management.internal.cli.functions.GetRegionsFunction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.data.gemfire.client.function.ListRegionsOnServerFunction;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * A Spring {@link BeanFactoryPostProcessor} used to register a Client Region Proxy bean for each Region
 * accessible to a Pivotal GemFire DataSource. If the Region is already defined, the bean definition will not be overridden.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
 * @see org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate
 * @see ListRegionsOnServerFunction
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientRegionFactory
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.management.internal.cli.functions.GetRegionsFunction
 * @since 1.2.0
 */
public class GemfireDataSourcePostProcessor implements BeanFactoryPostProcessor {

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	private final ClientCache clientCache;

	/**
	 * Constructs an instance of the GemfireDataSourcePostProcessor BeanFactoryPostProcessor class initialized
	 * with the specified Pivotal GemFire ClientCache instance for creating client PROXY Regions for all data Regions
	 * configured in the Pivotal GemFire cluster.
	 *
	 * @param clientCache the Pivotal GemFire ClientCache instance.
	 * @see org.apache.geode.cache.client.ClientCache
	 */
	public GemfireDataSourcePostProcessor(ClientCache clientCache) {

		Assert.notNull(clientCache, "ClientCache must not be null");

		this.clientCache = clientCache;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor#postProcessBeanFactory(org.springframework.beans.factory.config.ConfigurableListableBeanFactory)
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		createClientRegionProxies(beanFactory, regionNames());
	}

	// TODO: remove this logic and delegate to o.s.d.g.config.remote.GemfireAdminOperations
	Iterable<String> regionNames() {

		try {
			return execute(new ListRegionsOnServerFunction());
		}
		catch (Exception ignore) {
			try {

				Object results = execute(new GetRegionsFunction());

				List<String> regionNames = Collections.emptyList();

				if (containsRegionInformation(results)) {

					Object[] resultsArray = (Object[]) results;

					regionNames = new ArrayList<>(resultsArray.length);

					for (Object result : resultsArray) {
						regionNames.add(((RegionInformation) result).getName());
					}
				}

				return regionNames;
			}
			catch (Exception cause) {
				log("Failed to determine the Regions available on the Server: %n%1$s", cause);
				return Collections.emptyList();
			}
		}
	}

	@SuppressWarnings("unchecked")
	<T> T execute(Function gemfireFunction, Object... arguments) {
		return new GemfireOnServersFunctionTemplate(this.clientCache).executeAndExtract(gemfireFunction, arguments);
	}

	boolean containsRegionInformation(Object results) {
		return results instanceof Object[] && ((Object[]) results).length > 0
			&& ((Object[]) results)[0] instanceof RegionInformation;
	}

	void createClientRegionProxies(ConfigurableListableBeanFactory beanFactory, Iterable<String> regionNames) {

		if (regionNames.iterator().hasNext()) {

			ClientRegionFactory<?, ?> clientRegionFactory =
				this.clientCache.createClientRegionFactory(ClientRegionShortcut.PROXY);

			for (String regionName : regionNames) {

				boolean createRegion = true;

				if (beanFactory.containsBean(regionName)) {

					Object existingBean = beanFactory.getBean(regionName);

					if (logger.isWarnEnabled()) {
						logger.warn("Cannot create a client PROXY Region bean named {}; A bean with name {} having type {} already exists",
							regionName, regionName, ObjectUtils.nullSafeClassName(existingBean));
					}

					createRegion = false;
				}

				if (createRegion) {
					log("Creating Region bean with name '%s'...", regionName);
					beanFactory.registerSingleton(regionName, clientRegionFactory.create(regionName));
				}
				else {
					log("A Region with name '%s' is already defined", regionName);
				}
			}
		}
	}

	void log(String message, Object... arguments) {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format(message, arguments));
		}
	}
}
