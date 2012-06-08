/*
 * Copyright 2010-2012 the original author or authors.
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
package org.springframework.data.gemfire;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Region;

@SuppressWarnings("deprecation")
/**
 * FactoryBean for creating a Gemfire Region as a subregion
 * @author David Turanski
 *
 * @param <K> - Region Key Type
 * @param <V> - Region Value Type
 */
public class SubRegionFactoryBean<K, V> extends AttributesFactory<K, V> implements FactoryBean<Region<K, V>>,
		InitializingBean {

	protected final Log log = LogFactory.getLog(getClass());

	@SuppressWarnings("unused")
	private String name;

	private String regionName;

	private Region<K, V> subRegion;

	private Region<?, ?> parent;

	private boolean lookupOnly;

	@Override
	public void afterPropertiesSet() throws Exception {
		Assert.notNull(parent, "parent region must not be null");

		this.subRegion = parent.getSubregion(regionName);
		if (this.subRegion == null) {
			if (lookupOnly) {
				throw new BeanInitializationException("Cannot find region [" + regionName + "] in cache "
						+ parent.getRegionService());
			}
			else {
				log.debug("creating subregion of [" + parent.getFullPath() + "] with name " + regionName);
				this.subRegion = this.parent.createSubregion(regionName, create());
			}
		}
	}

	@Override
	public Region<K, V> getObject() throws Exception {
		return this.subRegion;
	}

	@Override
	public Class<?> getObjectType() {
		return Region.class;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Set the bean name - the same as the subregion full path
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Set the simple name of the region
	 * @param regionName
	 */
	public void setRegionName(String regionName) {
		this.regionName = regionName;
	}

	/**
	 * Set the parent Region
	 * @param parent
	 */
	public void setParent(Region<?, ?> parent) {
		this.parent = parent;
	}

	/**
	 * Set to true if the subregion should already exist, e.g., specified by
	 * &lt;lookup-region&gt;
	 * @param lookupOnly
	 */
	public void setLookupOnly(boolean lookupOnly) {
		this.lookupOnly = lookupOnly;
	}

}
