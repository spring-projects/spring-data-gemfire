/*
 * Copyright 2011-2013 the original author or authors.
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
import org.springframework.data.gemfire.GemfireOperations;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;

/**
 * Convenient super class for GemFire data access objects. Intended for
 * GemfireTemplate usage. 
 * 
 * <p>Requires a Region to be set, providing a GemfireTemplate based on it to subclasses. 
 * Can alternatively be initialized directly via a GemfireTemplate, to reuse the latter's
 * settings.
 *
 * <p>This class will create its own GemfireTemplate if an Region reference is passed in. 
 * A custom GemfireTemplate instance can be used through overriding <code>createGemfireTemplate</code>.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.dao.support.DaoSupport
 */
public class GemfireDaoSupport extends DaoSupport {

	private GemfireOperations gemfireTemplate;

	/**
	 * Sets the GemFire Cache Region to be used by this DAO. Will automatically create
	 * an instance of the GemfireTemplate for the given Region.
	 *
	 * @param region the GemFire Cache Region upon which this DAO operates.
	 * @see com.gemstone.gemfire.cache.Region
	 * @see #createGemfireTemplate(com.gemstone.gemfire.cache.Region)
	 */
	public void setRegion(Region<?, ?> region) {
		this.gemfireTemplate = createGemfireTemplate(region);
	}

	/**
	 * Set the GemfireTemplate for this DAO explicitly as an alternative to specifying a GemFire Cache {@link Region}.
	 *
	 * @param gemfireTemplate the GemfireTemplate to be use by this DAO.
	 * @see org.springframework.data.gemfire.GemfireOperations
	 * @see org.springframework.data.gemfire.GemfireTemplate
	 * @see #setRegion
	 */
	public final void setGemfireTemplate(GemfireOperations gemfireTemplate) {
		this.gemfireTemplate = gemfireTemplate;
	}

	/**
	 * Returns the GemfireTemplate for this DAO, pre-initialized with the Region or set explicitly.
	 *
	 * @return an instance of the GemfireTemplate to perform data access operations on the GemFire Cache Region.
	 * @see org.springframework.data.gemfire.GemfireOperations
	 * @see org.springframework.data.gemfire.GemfireTemplate
	 */
	public final GemfireOperations getGemfireTemplate() {
		return gemfireTemplate;
	}

	/**
	 * Creates an instance of the GemfireTemplate for the given GemFire Cache Region.
	 * <p>Can be overridden in subclasses to provide a GemfireTemplate instance with different configuration,
	 * or even a custom GemfireTemplate subclass.
	 *
	 * @param region the GemFire Cache Region for which the GemfireTemplate is created.
	 * @return a new GemfireTemplate instance configured with the given GemFire Cache Region.
	 * @see com.gemstone.gemfire.cache.Region
	 * @see #setRegion
	 */
	protected GemfireOperations createGemfireTemplate(Region<?, ?> region) {
		return new GemfireTemplate(region);
	}

	/**
	 * Verifies that this DAO has been configured properly.
	 */
	@Override
	protected final void checkDaoConfig() {
		Assert.state(gemfireTemplate != null, "A GemFire Cache Region or an instance of the GemfireTemplate is required.");
	}

}
