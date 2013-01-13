/*
 * Copyright 2010-2013 the original author or authors.
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

import com.gemstone.gemfire.cache.CacheLoader;
import com.gemstone.gemfire.cache.CacheLoaderException;
import com.gemstone.gemfire.cache.LoaderHelper;

/**
 * User object used for testing Spring wiring.
 * 
 * @author Costin Leau
 */
@SuppressWarnings("rawtypes")
public class UserObject extends WiringDeclarableSupport implements CacheLoader {

	public static UserObject THIS;

	private String prop1;

	private Object prop2;

	public UserObject() {
		System.out.println("Initialized");
		THIS = this;
	}

	@Override
	public Object load(LoaderHelper helper) throws CacheLoaderException {
		return new Object();
	}

	/**
	 * @return the prop1
	 */
	public String getProp1() {
		return prop1;
	}

	/**
	 * @param prop1 the prop1 to set
	 */
	public void setProp1(String prop1) {
		this.prop1 = prop1;
	}

	/**
	 * @return the prop2
	 */
	public Object getProp2() {
		return prop2;
	}

	/**
	 * @param prop2 the prop2 to set
	 */
	public void setProp2(Object prop2) {
		this.prop2 = prop2;
	}
}