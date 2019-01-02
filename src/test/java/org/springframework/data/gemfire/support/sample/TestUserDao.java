/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.support.sample;

import javax.sql.DataSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

/**
 * The TestUserDao class is an implementation of the UserDao Data Access Object (DAO) interface for performing
 * data access and persistence operations on Users.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.repository.sample.User
 * @see org.springframework.data.gemfire.support.sample.UserDao
 * @see org.springframework.stereotype.Repository
 * @since 1.4.0
 */
@Repository("userDao")
@SuppressWarnings("unused")
public class TestUserDao implements UserDao {

	@Autowired
	private DataSource userDataSource;

	public DataSource getDataSource() {
		Assert.state(userDataSource != null, "A reference to the Users DataSource was not properly configured!");
		return userDataSource;
	}
}
