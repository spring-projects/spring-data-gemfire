/*
 * Copyright 2002-2013 the original author or authors.
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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.function.execution.GemfireOnServersFunctionTemplate;
import org.springframework.data.gemfire.support.ListRegionsOnServerFunction;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.management.internal.cli.functions.ListFunctionFunction;

/**
 * A {@link BeanFactoryPostProcessor} to register a Client Region bean, if necessary, for each Region accessible
 * to a Gemfire data source. If the Region is already defined, the definition will not be overridden.
 *
 * @author David Turanski
 * @author John Blum
 */
public class GemfireDataSourcePostProcessor implements BeanFactoryPostProcessor {

	private static Log logger = LogFactory.getLog(GemfireDataSourcePostProcessor.class);

	private final ClientCache cache;

	public GemfireDataSourcePostProcessor(final ClientCache cache) {
		this.cache = cache;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor#postProcessBeanFactory(org.springframework.beans.factory.config.ConfigurableListableBeanFactory)
	 */
	@Override
	public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
		if (isFunctionAvailable(ListRegionsOnServerFunction.ID)) {
			createClientRegions(beanFactory);
		}
	}

	// TODO what happens when the GemFire cluster contains a mix of "pure" GemFire Servers (non-Spring configured)
	// as well as Spring configured/bootstrapped GemFire Servers?
	boolean isFunctionAvailable(String targetFunctionId) {
		GemfireOnServersFunctionTemplate functionTemplate = new GemfireOnServersFunctionTemplate(cache);

		Iterable<String> functionIds = CollectionUtils.nullSafeIterable(
			functionTemplate.<Iterable<String>>executeAndExtract(new ListFunctionFunction()));

		for (String functionId : functionIds) {
			if (functionId.equals(targetFunctionId)) {
				return true;
			}
		}

		return false;
	}

	/* (non-Javadoc) */
	void createClientRegions(ConfigurableListableBeanFactory beanFactory) {
		GemfireOnServersFunctionTemplate functionTemplate = new GemfireOnServersFunctionTemplate(cache);

		Iterable<String> regionNames = CollectionUtils.nullSafeIterable(
			functionTemplate.<Iterable<String>>executeAndExtract(new ListRegionsOnServerFunction()));

		if (regionNames.iterator().hasNext()) {
			ClientRegionFactory<?, ?> clientRegionFactory = cache.createClientRegionFactory(ClientRegionShortcut.PROXY);

			for (String regionName : regionNames) {
				boolean createRegion = true;

				if (beanFactory.containsBean(regionName)) {
					Object existingBean = beanFactory.getBean(regionName);

					Assert.isTrue(existingBean instanceof Region, String.format(
						"Cannot create a client Region bean named '%1$s'. A bean with this name of type '%2$s' already exists.",
						regionName, ObjectUtils.nullSafeClassName(existingBean)));

					createRegion = false;
				}

				if (createRegion) {
					log("Creating Region bean with name '%s'...", regionName);
					beanFactory.registerSingleton(regionName, clientRegionFactory.create(regionName));
				}
				else {
					log("A Region with name '%s' is already defined.", regionName);
				}
			}
		}
	}

	/* (non-Javadoc) */
	private void log(String message, Object... arguments) {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format(message, arguments));
		}
	}

}

