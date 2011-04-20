/*
 * Copyright 2011 the original author or authors.
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

package org.springframework.data.gemfire.support;

import org.springframework.dao.support.DaoSupport;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;

/**
 * Convenient super class for GemFire data access objects. Intended for
 * GemfireTemplate usage. 
 * 
 * <p>Requires an EntityManagerFactory or EntityManager to be set,
 * providing a JpaTemplate based on it to subclasses. Can alternatively
 * be initialized directly via a JpaTemplate, to reuse the latter's
 * settings such as the EntityManagerFactory, JpaDialect, flush mode, etc.
 *
 * <p>This class will create its own GemfireTemplate if an EntityManagerFactory
 * or EntityManager reference is passed in. A custom JpaTemplate instance
 * can be used through overriding <code>createJpaTemplate</code>.
 * 
 * @author Costin Leau
 */
public class GemfireDaoSupport extends DaoSupport {

	private GemfireTemplate gemfireTemplate;

	/**
	 * Sets the GemFire Region to be used by this DAO.
	 * Will automatically create a GemfireTemplate for the given Region.
	 *
	 * @param region
	 */
	public void setRegion(Region<?, ?> region) {
		this.gemfireTemplate = createGemfireTemplate(region);
	}

	/**
	 * Creates a GemfireTemplate for the given Region.
	 * <p>Can be overridden in subclasses to provide a GemfireTemplate instance
	 * with different configuration, or a custom GemfireTemplate subclass.
	 * @param region the GemFire Region to create a GemfireTemplate for
	 * @return the new GemfireTemplate instance
	 * @see #setRegion
	 */
	protected GemfireTemplate createGemfireTemplate(Region<?, ?> region) {
		return new GemfireTemplate(region);
	}

	/**
	 * Set the GemfireTemplate for this DAO explicitly,
	 * as an alternative to specifying a GemFire {@link Region}.
	 * @see #setRegion
	 */
	public final void setGemfireTemplate(GemfireTemplate gemfireTemplate) {
		this.gemfireTemplate = gemfireTemplate;
	}

	/**
	 * Return the GemfireTemplate for this DAO, pre-initialized
	 * with the Region or set explicitly.
	 */
	public final GemfireTemplate getGemfireTemplate() {
		return gemfireTemplate;
	}

	@Override
	protected final void checkDaoConfig() {
		Assert.notNull(gemfireTemplate, "region or gemfireTemplate is required");
	}
}